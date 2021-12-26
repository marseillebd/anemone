{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}

module Language.Bslisp.TreeWalk.Eval
  ( eval
  ) where


import Control.DeepSeq (force)
import Control.Monad (forM_)
import Control.Monad.Trans.State.Strict (StateT,gets,modify',state,evalStateT)
import Data.Bifunctor (second)
import Data.Functor ((<&>))
import Data.List.NonEmpty (NonEmpty(..))
import Data.List.Reverse (RList,snoc)
import Data.Symbol (intern,unintern)
import Data.Zexpr.Location (Loc(..))
import Data.Zexpr.Sexpr (Atom(..),Sexpr(..))
import Language.Bslisp.TreeWalk.Environment (Env,Binding(..))
import Language.Bslisp.TreeWalk.Environment (valueNamespace)
import Language.Bslisp.TreeWalk.Stack (Stack,StackItem(..),PushPop(..),ReturnFrom(..))
import Language.Bslisp.TreeWalk.Value (Control(..),capture,PrimExn(..))
import Language.Bslisp.TreeWalk.Value (PrimCaseBin(..),PrimCaseQuat(..))
import Language.Bslisp.TreeWalk.Value (PrimOp(..),PrimAp(..))
import Language.Bslisp.TreeWalk.Value (PrimUnary(..),PrimBin(..))
import Language.Bslisp.TreeWalk.Value (Value(..),Callable(..),toCallable,Closure(..))

import qualified Data.List.NonEmpty as NE
import qualified Data.List.Reverse as RList
import qualified Data.Text as T
import qualified Language.Bslisp.Keywords as Keyword
import qualified Language.Bslisp.TreeWalk.Environment as Env
import qualified Language.Bslisp.TreeWalk.Stack as Stack

data Machine = M
  { stack :: !Stack
  , env :: !Env
  }

eval :: [Sexpr] -> Env -> IO (Either Control Value)
eval stmts env =
  let m = M{stack=Stack.empty,env}
   in evalStateT go m
  where
  go = case stmts of
    [] -> pure . Right $ NilVal
    [expr] -> elaborate expr >>= reduce
    (expr:s:ss) -> do
      modify' . push $ Sequence (s:|ss)
      elaborate expr >>= reduce

-- WARNING elaborate simply finds an atomic redex, so it should only tail-call itself or pure
elaborate :: Sexpr -> StateT Machine IO (Either Control Value)
elaborate = \case
  SAtom _ (Int n) -> pure . Right $ IntVal n
  SAtom _ (Str s) -> pure . Right $ StrVal s
  SAtom loc (Sym x) -> do
    env0 <- gets env
    val_m <- Env.lookup env0 valueNamespace x
    pure $ case val_m of
      Nothing -> Left $ PrimCtrl RList.nil (ScopeExn loc valueNamespace x)
      Just Bound{value} -> Right value
  SCombo _ [] -> pure . Right $ NilVal
  SCombo loc combo@(SAtom _ (Sym x) : es)
    | x == Keyword.operate -> case es of
      [] -> reduce . Left $ PrimCtrl RList.nil (SyntaxExn (SCombo loc combo) "expecting operative")
      op:sexprs -> do
        modify' . push $ Operate loc sexprs
        elaborate op
  SCombo _ [e] -> elaborate e
  SCombo loc (f:arg:args) -> do
    modify' . push $ Args loc (arg:|args)
    elaborate f

reduce :: Either Control Value -> StateT Machine IO (Either Control Value)
reduce (Right v) = state pop >>= maybe (pure $ Right v) \case
  k@(Operate loc sexprs) -> case toCallable v of
    Just f -> gets env >>= \inEnv -> operate k f inEnv loc sexprs
    Nothing -> raise PrimCtrl k (UncallableExn v)
  k@(Args calledAt (arg:|args)) -> case toCallable v of
    Just f -> do
      modify' . push $ Apply calledAt f args
      elaborate arg >>= reduce
    Nothing -> raise PrimCtrl k (UncallableExn v)
  k@(ArgVal calledAt arg) -> case toCallable v of
    Just f -> apply k calledAt f arg
    Nothing -> raise PrimCtrl k (UncallableExn v)
  k@(Apply1 calledAt f) -> apply k calledAt f v
  Restore _ env' -> do
    modify' $ \st ->st{env=env'}
    reduce . Right $ v
  Then stmt -> do
    !_ <- pure $! force v
    elaborate stmt >>= reduce
  OpDefine inEnv _ x -> do
    Env.define inEnv valueNamespace x v -- TODO associate a location with the definition
    reduce . Right $ NilVal
  OpList vs [] -> reduce . Right $ ListVal (RList.toList $ vs `snoc` v)
  OpList vs (e:es) -> do
    modify' . push $ OpList (vs `snoc` v) es
    elaborate e >>= reduce
  PrimArg _ _ _ -> error "internal error: popped `PrimArg` continuation while reducing"
reduce (Left exn) = state unsafePop >>= maybe (pure $ Left exn) \case
  -- TODO catch exceptions
  k -> reduce . Left $ exn `capture` k

-- WARNING when reduce calls apply, it needs apply to tail-call reduce, since apply might alter the stack dramatically (e.g. if there's a continuation capture)
apply :: StackItem 'Pop -> Loc -> Callable -> Value -> StateT Machine IO (Either Control Value)
apply k _ op@(OperPrim _) _ = raise PrimCtrl k (UnexpectedOperative op)
apply _ _ (CallPrim PrimEval) a = reduce . Right $ PrimAp (PrimEval1 a)
apply _ calledAt (CallPrim (PrimEval1 a)) b = case (a, b) of
  (EnvVal evaleeEnv, SexprVal evalee) -> do
    let trace = FromEval{evaledAt=calledAt,evaleeEnv,evalee}
    modify' . push =<< Restore trace <$> gets env
    modify' $ \st -> st{env=evaleeEnv}
    elaborate evalee >>= reduce
  (EnvVal _, _) -> raisePrimArg 2 PrimEval (a:|[b]) (TypeError b)
  (_, _) -> raisePrimArg 1 PrimEval (a:|[b]) (TypeError a)
apply _ _ (CallPrim PrimDefineIn) a = case a of
  EnvVal inEnv -> reduce . Right $ PrimAp (PrimDefineIn3 inEnv)
  _ -> raisePrimArg 1 PrimDefineIn (a:|[]) (TypeError a)
apply _ _ (CallPrim (PrimDefineIn3 inEnv)) b = case b of
  SymVal ns -> reduce . Right $ PrimAp (PrimDefineIn2 inEnv ns)
  _ -> raisePrimArg 2 PrimDefineIn (EnvVal inEnv:|[b]) (TypeError b)
apply _ _ (CallPrim (PrimDefineIn2 inEnv ns)) c = case c of
  SymVal x -> reduce . Right $ PrimAp (PrimDefineIn1 inEnv ns x)
  _ -> raisePrimArg 3 PrimDefineIn (EnvVal inEnv:|[SymVal ns,c]) (TypeError c)
apply _ _ (CallPrim (PrimDefineIn1 inEnv ns x)) v = do
  Env.define inEnv ns x v
  reduce . Right $ NilVal
apply _ _ (CallPrim (PrimUnary prim)) a = case evalPrimUnary prim a of
  Right v -> reduce . Right $ v
  Left exn -> raisePrimArg 1 (PrimUnary prim) (a:|[]) exn
apply _ _ (CallPrim (PrimBin prim)) a = reduce . Right $ PrimAp (PrimBin1 prim a)
apply _ _ (CallPrim (PrimBin1 prim a)) b = case evalPrimBin prim a b of
  Right v -> reduce . Right $ v
  Left (n, exn) -> raisePrimArg n (PrimBin prim) (a:|[b]) exn
apply _ _ (CallPrim (PrimCaseBin prim)) v = reduce . Right $ PrimAp (PrimCaseBin2 prim v)
apply _ _ (CallPrim (PrimCaseBin2 prim v)) a = reduce . Right $ PrimAp (PrimCaseBin1 prim v a)
apply _ calledAt (CallPrim (PrimCaseBin1 prim v a)) b = evalPrimCaseBin calledAt prim v a b
apply _ _ (CallPrim (PrimCaseQuat prim)) v = reduce . Right $ PrimAp (PrimCaseQuat4 prim v)
apply _ _ (CallPrim (PrimCaseQuat4 prim v)) a = reduce . Right $ PrimAp (PrimCaseQuat3 prim v a)
apply _ _ (CallPrim (PrimCaseQuat3 prim v a)) b = reduce . Right $ PrimAp (PrimCaseQuat2 prim v a b)
apply _ _ (CallPrim (PrimCaseQuat2 prim v a b)) c = reduce . Right $ PrimAp (PrimCaseQuat1 prim v a b c)
apply _ calledAt (CallPrim (PrimCaseQuat1 prim v a b c)) d = evalPrimCaseQuat calledAt prim v a b c d
apply _ calledAt (CallClosure f@Closure{scope,args,params,body}) arg = case params of
  param:|(p:ps) -> reduce . Right $ ClosureVal f{args=args `snoc` (param,arg), params=p:|ps}
  param:|[] -> do
    let args' = args `snoc` (param,arg)
        params0 = case RList.toList (fst <$> args) of
          [] -> params
          (a:as) -> a :| (as ++ NE.toList params)
        callee = f{args=RList.nil,params=params0} :: Closure -- a version of the closure with arguments/parameters reset
    calleeEnv <- Env.newChild scope
    forM_ args' $ \(x, v) -> Env.define calleeEnv valueNamespace x v
    let trace = FromCall
          { calledAt
          , callee
          , args = snd <$> RList.toList args'
          }
    callerEnv <- gets env
    modify' . push $ Restore trace callerEnv
    modify' $ \st -> st{env=calleeEnv}
    elaborate body >>= reduce

applyImmediate :: Loc -> Value -> NonEmpty Value -> StateT Machine IO (Either Control Value)
applyImmediate calledAt f args = do
  modify' . push $ ArgVals calledAt args
  reduce (Right f)

-- WARNING as for apply, when reduce calls operate, it needs operate to tail-call reduce
operate :: StackItem 'Pop -> Callable -> Env -> Loc -> [Sexpr] -> StateT Machine IO (Either Control Value)
operate _ (OperPrim PrimLambda) env loc sexprs = case sexprs of
  [paramSexprs, body] -> case toParamList paramSexprs of
    Just params -> do
      reduce . Right $ ClosureVal Closure
        { name = Nothing
        , definedAt = loc
        , scope = env
        , args = RList.nil
        , params
        , body
        }
    Nothing -> reduce . Left $ PrimCtrl RList.nil (SyntaxExn paramSexprs "expecting parameter list")
  [] -> reduce . Left $ PrimCtrl RList.nil (SyntaxExn (SCombo loc sexprs) "expecting parameter list and function body")
  [_] -> reduce . Left $ PrimCtrl RList.nil (SyntaxExn (SCombo loc sexprs) "expecting function body")
  (_:_:extra:_) -> reduce . Left $ PrimCtrl RList.nil (SyntaxExn extra "unexpected s-expr after function body")
  where
  -- TODO toParamList should specify the offending item rather than just saying the whole thing is bad
  toParamList (SAtom _ _) = Nothing
  toParamList (SCombo _ params) = go params
    where
    go [] = Nothing
    go [param] = go1 param <&> (:|[])
    go (param:rest) = NE.cons <$> go1 param <*> go rest
    go1 (SAtom _ (Sym x)) = Just x
    go1 _ = Nothing
operate _ (OperPrim PrimSequence) _ loc sexprs = case sexprs of
  [] -> reduce . Left $ PrimCtrl RList.nil (SyntaxExn (SCombo loc sexprs) "expecting one or more statements")
  [stmt] -> elaborate stmt >>= reduce
  (stmt:s:ss) -> do
    modify' . push $ Sequence (s:|ss)
    elaborate stmt >>= reduce
operate _ (OperPrim PrimDefine) env loc sexprs = case sexprs of
  [SAtom _ (Sym x), expr] -> do
    modify' . push $ OpDefine env loc x
    elaborate expr >>= reduce
  [] -> reduce . Left $ PrimCtrl RList.nil (SyntaxExn (SCombo loc sexprs) "expecting symbol")
  [_] -> reduce . Left $ PrimCtrl RList.nil (SyntaxExn (SCombo loc sexprs) "expecting definition body")
  [nonSymbol, _] -> reduce . Left $ PrimCtrl RList.nil (SyntaxExn nonSymbol "expecting symbol")
  (_:_:extra:_) -> reduce . Left $ PrimCtrl RList.nil (SyntaxExn extra "unexpected s-expr after definition body")
operate _ (OperPrim PrimList) _ _ sexprs = case sexprs of
  [] -> reduce . Right $ ListVal []
  (x:xs) -> do
    modify' . push $ OpList RList.nil xs
    elaborate x >>= reduce
operate k ap@(CallPrim _) _ _ _ = raise PrimCtrl k (UnexpectedApplicative ap)
operate _ (CallClosure f) env loc sexprs =
  let args = EnvVal env :| [LocVal loc, ListVal $ SexprVal <$> sexprs]
   in applyImmediate loc (ClosureVal f) args

evalPrimUnary :: PrimUnary -> Value -> Either PrimExn Value
evalPrimUnary PrimSexprIntro (IntVal n) = Right . SexprVal $ SAtom LocUnknown (Int n)
evalPrimUnary PrimSexprIntro (StrVal s) = Right . SexprVal $ SAtom LocUnknown (Str s)
evalPrimUnary PrimSexprIntro (SymVal x) = Right . SexprVal $ SAtom LocUnknown (Sym x)
evalPrimUnary PrimSexprIntro (ListVal xs0)
  | Just sexprs <- fromSexprList xs0 = Right . SexprVal $ SCombo LocUnknown sexprs
  where
  fromSexprList [] = Just []
  fromSexprList ((SexprVal sexpr) : xs) = (sexpr :) <$> fromSexprList xs
  fromSexprList _ = Nothing
evalPrimUnary PrimSexprIntro v = Left $ TypeError v
evalPrimUnary PrimSymIntro (StrVal s) = Right $ SymVal (intern $ T.unpack s)
evalPrimUnary PrimSymIntro v = Left $ TypeError v
evalPrimUnary PrimSymElim (SymVal x) = Right $ StrVal (T.pack $ unintern x)
evalPrimUnary PrimSymElim v = Left $ TypeError v


evalPrimBin :: PrimBin -> Value -> Value -> Either (Int, PrimExn) Value
evalPrimBin PrimAdd (IntVal a) (IntVal b) = Right $ IntVal (a + b)
evalPrimBin PrimAdd (IntVal _) v = Left (2, TypeError v)
evalPrimBin PrimAdd v _ = Left (1, TypeError v)
evalPrimBin PrimCons x (ListVal xs) = Right $ ListVal (x : xs)
evalPrimBin PrimCons _ v = Left (2, TypeError v)
evalPrimBin PrimUpdName (SymVal x) v = case v of
  ClosureVal f@Closure{} -> Right $ ClosureVal f{name = Just x}
  _ -> Right v
evalPrimBin PrimUpdName v _ = Left (1, TypeError v)
evalPrimBin PrimUpdLoc (LocVal loc) v = case v of
  ClosureVal f@Closure{} -> Right $ ClosureVal f{definedAt = loc}
  SexprVal (SAtom _ atom) -> Right $ SexprVal (SAtom loc atom)
  SexprVal (SCombo _ sexprs) -> Right $ SexprVal (SCombo loc sexprs)
  _ -> Left (2, TypeError v)
evalPrimBin PrimUpdLoc v _ = Left (1, TypeError v)

evalPrimCaseBin :: Loc
                -> PrimCaseBin -> Value
                -> Value -> Value
                -> StateT Machine IO (Either Control Value)
evalPrimCaseBin calledAt PrimUncons (ListVal lst) onNull onCons = case lst of
  [] -> applyImmediate calledAt onNull $ NilVal :| []
  (x:xs) -> applyImmediate calledAt onCons $ x :| [ListVal xs]
evalPrimCaseBin _ PrimUncons v a b =
  raisePrimArg 1 (PrimCaseBin PrimUncons) (v:|[a,b]) (TypeError v)

evalPrimCaseQuat :: Loc
                 -> PrimCaseQuat -> Value
                 -> Value -> Value -> Value -> Value
                 -> StateT Machine IO (Either Control Value)
evalPrimCaseQuat calledAt PrimSexprElim (SexprVal sexpr) onInt onStr onSym onCombo = case sexpr of
  SAtom _ (Int n) -> applyImmediate calledAt onInt $ IntVal n :| []
  SAtom _ (Str s) -> applyImmediate calledAt onStr $ StrVal s :| []
  SAtom _ (Sym x) -> applyImmediate calledAt onSym $ SymVal x :| []
  SCombo _ sexprs -> applyImmediate calledAt onCombo $ ListVal (SexprVal <$> sexprs) :| []
evalPrimCaseQuat _ PrimSexprElim v a b c d =
  raisePrimArg 1 (PrimCaseQuat PrimSexprElim) (v:|[a,b,c,d]) (TypeError v)

pop :: Machine -> (Maybe (StackItem 'Pop), Machine)
pop st@M{stack} = second (\stack' -> st{stack=stack'}) (Stack.pop stack)

push :: StackItem 'Push -> Machine -> Machine
push k st@M{stack} = st{stack=Stack.push k stack}

unsafePop :: Machine -> (Maybe (StackItem 'Push), Machine)
unsafePop st@M{stack} = second (\stack' -> st{stack=stack'}) (Stack.unsafePop stack)

raise :: (RList (StackItem 'Push) -> a -> Control) -> StackItem 'Pop -> a -> StateT Machine IO (Either Control Value)
raise mk k exn = reduce . Left $ mk (RList.singleton $ Stack.toPush k) exn

raisePrimArg :: Int -> PrimAp -> NonEmpty Value -> PrimExn -> StateT Machine IO (Either Control Value)
raisePrimArg n prim vs exn =
  let k = RList.singleton $ PrimArg n prim vs
   in reduce . Left $ PrimCtrl k exn
