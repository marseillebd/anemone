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
import Data.Functor ((<&>))
import Data.List.NonEmpty (NonEmpty(..))
import Data.List.Reverse (RList,snoc)
import Data.Symbol (intern,unintern)
import Data.Zexpr.Location (Loc(..))
import Data.Zexpr.Sexpr (Atom(..),Sexpr(..))
import Language.Bslisp.TreeWalk.Environment (Env(..),Binding(..))
import Language.Bslisp.TreeWalk.Environment (valueNamespace)
import Language.Bslisp.TreeWalk.Machine (currentEnv,enterEnv,returnToEnv)
import Language.Bslisp.TreeWalk.Machine (Eval,runEval,val,ctrl)
import Language.Bslisp.TreeWalk.Machine (pop,push,remergePop)
import Language.Bslisp.TreeWalk.Stack (StackItem(..),PushPop(..),ReturnFrom(..))
import Language.Bslisp.TreeWalk.Value (Control(..),capture,PrimExn(..))
import Language.Bslisp.TreeWalk.Value (PrimCaseBin(..),PrimCaseQuat(..))
import Language.Bslisp.TreeWalk.Value (PrimOp(..),PrimAp(..))
import Language.Bslisp.TreeWalk.Value (PrimUnary(..),PrimBin(..))
import Language.Bslisp.TreeWalk.Value (Value(..),Callable(..),toCallable,Closure(..))

import qualified Data.List.NonEmpty as NE
import qualified Data.List.Reverse as R
import qualified Data.Text as T
import qualified Language.Bslisp.Keywords as Keyword
import qualified Language.Bslisp.TreeWalk.Environment as Env
import qualified Language.Bslisp.TreeWalk.Stack as Stack

eval :: [Sexpr] -> Env -> IO (Either Control Value)
eval stmts env = runEval env $ case stmts of
  [] -> val NilVal
  [expr] -> elaborate expr >>= loop
  (expr:s:ss) -> do
    push $ Sequence (s:|ss)
    elaborate expr >>= loop
  where
  loop r = pop >>= \case
    Nothing -> pure r
    Just k -> reduce k r >>= loop


elaborate :: Sexpr -> Eval (Either Control Value)
elaborate = \case
  SAtom _ (Int n) -> val $ IntVal n
  SAtom _ (Str s) -> val $ StrVal s
  SAtom loc (Sym x) -> do
    env0 <- currentEnv
    Env.lookup env0 valueNamespace x >>= \case
      Just Bound{value} -> val value
      Nothing -> ctrl $ PrimCtrl R.nil (ScopeExn loc valueNamespace x)
  SCombo _ [] -> val NilVal
  SCombo loc combo@(SAtom _ (Sym x) : es)
    | x == Keyword.operate -> case es of
      [] -> ctrl $ PrimCtrl R.nil $ SyntaxExn (SCombo loc combo) "expecting operative"
      op:sexprs -> do
        push $ Operate loc sexprs
        elaborate op
  SCombo _ [e] -> elaborate e
  SCombo loc (f:arg:args) -> do
    push $ Args loc (arg:|args)
    elaborate f

reduce :: StackItem 'Pop -> Either Control Value -> Eval (Either Control Value)
reduce k (Right v) = case k of
  Operate loc sexprs -> case toCallable v of
    Just f -> currentEnv >>= \inEnv -> operate k f inEnv loc sexprs
    Nothing -> raise PrimCtrl k (UncallableExn v)
  Args calledAt (arg:|args) -> case toCallable v of
    Just f -> do
      push $ Apply calledAt f args
      elaborate arg
    Nothing -> raise PrimCtrl k (UncallableExn v)
  ArgVal calledAt arg -> case toCallable v of
    Just f -> apply k calledAt f arg
    Nothing -> raise PrimCtrl k (UncallableExn v)
  Apply1 calledAt f -> apply k calledAt f v
  Restore _ env0 -> do
    returnToEnv env0
    val v
  Then stmt -> do
    !_ <- pure $! force v
    elaborate stmt
  OpDefine inEnv _ x -> do
    Env.define inEnv valueNamespace x v
    val v
  OpList vs [] -> val $ ListVal $ R.toList (vs `snoc` v)
  OpList vs (e:es) -> do
    push $ OpList (vs `snoc` v) es
    elaborate e
  PrimArg _ _ _ -> error "internal error: popped `PrimArg` continuation while reducing"
reduce k1 (Left exn) = do
  k <- remergePop k1
  -- TODO catch exceptions
  ctrl $ exn `capture` k

apply :: StackItem 'Pop -> Loc -> Callable -> Value -> Eval (Either Control Value)
apply k _ op@(OperPrim _) _ = raise PrimCtrl k (UnexpectedOperative op)
apply _ _ (CallPrim PrimEval) a = val (PrimAp (PrimEval1 a))
apply _ calledAt (CallPrim (PrimEval1 a)) b = case (a, b) of
  (EnvVal evaleeEnv, SexprVal evalee) -> do
    let trace = FromEval{evaledAt=calledAt,evaleeEnv,evalee}
    enterEnv trace evaleeEnv
    elaborate evalee
  (EnvVal _, _) -> raisePrimArg 2 PrimEval (a:|[b]) (TypeError b)
  (_, _) -> raisePrimArg 1 PrimEval (a:|[b]) (TypeError a)
apply _ _ (CallPrim PrimDefineIn) a = case a of
  EnvVal inEnv -> val $ PrimAp (PrimDefineIn3 inEnv)
  _ -> raisePrimArg 1 PrimDefineIn (a:|[]) (TypeError a)
apply _ _ (CallPrim (PrimDefineIn3 inEnv)) b = case b of
  SymVal ns -> val $ PrimAp (PrimDefineIn2 inEnv ns)
  _ -> raisePrimArg 2 PrimDefineIn (EnvVal inEnv:|[b]) (TypeError b)
apply _ _ (CallPrim (PrimDefineIn2 inEnv ns)) c = case c of
  SymVal x -> val $ PrimAp (PrimDefineIn1 inEnv ns x)
  _ -> raisePrimArg 3 PrimDefineIn (EnvVal inEnv:|[SymVal ns,c]) (TypeError c)
apply _ _ (CallPrim (PrimDefineIn1 inEnv ns x)) v = do
  Env.define inEnv ns x v
  val v
apply _ _ (CallPrim (PrimUnary prim)) a = case evalPrimUnary prim a of
  Right v -> val v
  Left exn -> raisePrimArg 1 (PrimUnary prim) (a:|[]) exn
apply _ _ (CallPrim (PrimBin prim)) a = val $ PrimAp (PrimBin1 prim a)
apply _ _ (CallPrim (PrimBin1 prim a)) b = case evalPrimBin prim a b of
  Right v -> val v
  Left (n, exn) -> raisePrimArg n (PrimBin prim) (a:|[b]) exn
apply _ _ (CallPrim (PrimCaseBin prim)) v = val $ PrimAp (PrimCaseBin2 prim v)
apply _ _ (CallPrim (PrimCaseBin2 prim v)) a = val $ PrimAp (PrimCaseBin1 prim v a)
apply _ calledAt (CallPrim (PrimCaseBin1 prim v a)) b = evalPrimCaseBin calledAt prim v a b
apply _ _ (CallPrim (PrimCaseQuat prim)) v = val $ PrimAp (PrimCaseQuat4 prim v)
apply _ _ (CallPrim (PrimCaseQuat4 prim v)) a = val $ PrimAp (PrimCaseQuat3 prim v a)
apply _ _ (CallPrim (PrimCaseQuat3 prim v a)) b = val $ PrimAp (PrimCaseQuat2 prim v a b)
apply _ _ (CallPrim (PrimCaseQuat2 prim v a b)) c = val $ PrimAp (PrimCaseQuat1 prim v a b c)
apply _ calledAt (CallPrim (PrimCaseQuat1 prim v a b c)) d = evalPrimCaseQuat calledAt prim v a b c d
apply _ calledAt (CallClosure f@Closure{scope,args,params,body}) arg = case params of
  param:|(p:ps) -> val $ ClosureVal f{args=args `snoc` (param,arg), params=p:|ps}
  param:|[] -> do
    let args' = args `snoc` (param,arg)
        params0 = case R.toList (fst <$> args) of
          [] -> params
          (a:as) -> a :| (as ++ NE.toList params)
        callee = f{args=R.nil,params=params0} :: Closure -- a version of the closure with arguments/parameters reset
    calleeEnv <- Env.newChild scope <&> \env -> env{createdAt=Just calledAt}
    forM_ args' $ \(x, v) -> Env.define calleeEnv valueNamespace x v
    let trace = FromCall
          { calledAt
          , callee
          , args = snd <$> R.toList args'
          }
    enterEnv trace calleeEnv
    elaborate body

applyImmediate :: Loc -> Value -> NonEmpty Value -> Eval (Either Control Value)
applyImmediate calledAt f args = do
  push $ ArgVals calledAt args
  val f

operate :: StackItem 'Pop -> Callable -> Env -> Loc -> [Sexpr] -> Eval (Either Control Value)
operate _ (OperPrim PrimLambda) env loc sexprs = case sexprs of
  [paramSexprs, body] -> case toParamList paramSexprs of
    Just params -> val $ ClosureVal Closure
      { name = Nothing
      , definedAt = loc
      , scope = env
      , args = R.nil
      , params
      , body
      }
    Nothing -> ctrl $ PrimCtrl R.nil $ SyntaxExn paramSexprs "expecting parameter list"
  [] -> ctrl $ PrimCtrl R.nil $ SyntaxExn (SCombo loc sexprs) "expecting parameter list and function body"
  [_] -> ctrl $ PrimCtrl R.nil $ SyntaxExn (SCombo loc sexprs) "expecting function body"
  (_:_:extra:_) -> ctrl $ PrimCtrl R.nil $ SyntaxExn extra "unexpected s-expr after function body"
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
  [] -> ctrl $ PrimCtrl R.nil $ SyntaxExn (SCombo loc sexprs) "expecting one or more statements"
  [stmt] -> elaborate stmt
  (stmt:s:ss) -> do
    push $ Sequence (s:|ss)
    elaborate stmt
operate _ (OperPrim PrimDefine) env loc sexprs = case sexprs of
  [SAtom _ (Sym x), expr] -> do
    push $ OpDefine env loc x
    elaborate expr
  [] -> ctrl $ PrimCtrl R.nil $ SyntaxExn (SCombo loc sexprs) "expecting symbol"
  [_] -> ctrl $ PrimCtrl R.nil $ SyntaxExn (SCombo loc sexprs) "expecting definition body"
  [nonSymbol, _] -> ctrl $ PrimCtrl R.nil $ SyntaxExn nonSymbol "expecting symbol"
  (_:_:extra:_) -> ctrl $ PrimCtrl R.nil $ SyntaxExn extra "unexpected s-expr after definition body"
operate _ (OperPrim PrimList) _ _ sexprs = case sexprs of
  [] -> val $ ListVal []
  (x:xs) -> do
    push $ OpList R.nil xs
    elaborate x
operate k ap@(CallPrim _) _ _ _ = raise PrimCtrl k (UnexpectedApplicative ap)
operate _ (CallClosure f) env loc sexprs =
  let args = EnvVal env :| [LocVal loc, ListVal $ SexprVal <$> sexprs]
   in applyImmediate loc (ClosureVal f) args

raise :: (RList (StackItem 'Push) -> a -> Control) -> StackItem 'Pop -> a -> Eval (Either Control Value)
raise mk k exn = ctrl $ mk (R.singleton $ Stack.toPush k) exn

raisePrimArg :: Int -> PrimAp -> NonEmpty Value -> PrimExn -> Eval (Either Control Value)
raisePrimArg n prim vs exn =
  let k = R.singleton $ PrimArg n prim vs
   in ctrl $ PrimCtrl k exn












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
  ClosureVal f -> Right $ ClosureVal f{name = Just x}
  EnvVal e -> Right $ EnvVal e{name = Just x}
  _ -> Right v
evalPrimBin PrimUpdName v _ = Left (1, TypeError v)
evalPrimBin PrimUpdLoc (LocVal loc) v = case v of
  SexprVal (SAtom _ atom) -> Right $ SexprVal (SAtom loc atom)
  SexprVal (SCombo _ sexprs) -> Right $ SexprVal (SCombo loc sexprs)
  ClosureVal f -> Right $ ClosureVal f{definedAt = loc}
  EnvVal e -> Right $ EnvVal e{createdAt = Just loc}
  _ -> Left (2, TypeError v)
evalPrimBin PrimUpdLoc v _ = Left (1, TypeError v)

evalPrimCaseBin :: Loc
                -> PrimCaseBin -> Value
                -> Value -> Value
                -> Eval (Either Control Value)
evalPrimCaseBin calledAt PrimUncons (ListVal lst) onNull onCons = case lst of
  [] -> applyImmediate calledAt onNull $ NilVal :| []
  (x:xs) -> applyImmediate calledAt onCons $ x :| [ListVal xs]
evalPrimCaseBin _ PrimUncons v a b =
  raisePrimArg 1 (PrimCaseBin PrimUncons) (v:|[a,b]) (TypeError v)

evalPrimCaseQuat :: Loc
                 -> PrimCaseQuat -> Value
                 -> Value -> Value -> Value -> Value
                 -> Eval (Either Control Value)
evalPrimCaseQuat calledAt PrimSexprElim (SexprVal sexpr) onInt onStr onSym onCombo = case sexpr of
  SAtom _ (Int n) -> applyImmediate calledAt onInt $ IntVal n :| []
  SAtom _ (Str s) -> applyImmediate calledAt onStr $ StrVal s :| []
  SAtom _ (Sym x) -> applyImmediate calledAt onSym $ SymVal x :| []
  SCombo _ sexprs -> applyImmediate calledAt onCombo $ ListVal (SexprVal <$> sexprs) :| []
evalPrimCaseQuat _ PrimSexprElim v a b c d =
  raisePrimArg 1 (PrimCaseQuat PrimSexprElim) (v:|[a,b,c,d]) (TypeError v)
