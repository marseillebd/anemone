{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ViewPatterns #-}

module Language.Anemone.TreeWalk.Eval
  ( eval
  ) where


import Control.DeepSeq (force)
import Control.Monad (forM_)
import Control.Monad.IO.Class (liftIO)
import Data.Bifunctor (second)
import Data.Foldable (toList)
import Data.Functor ((<&>))
import Data.IORef (newIORef,readIORef,writeIORef)
import Data.List.NonEmpty (NonEmpty(..))
import Data.List.Reverse (RList,snoc)
import Data.Sequence (Seq(..))
import Data.Symbol (intern,unintern)
import Data.Zexpr.Location (Loc(..))
import Data.Zexpr.Sexpr (Atom(..),Sexpr(..))
import Language.Anemone.TreeWalk.Environment (Env(..),Binding(..))
import Language.Anemone.TreeWalk.Environment (valueNamespace)
import Language.Anemone.TreeWalk.Machine (currentEnv,enterEnv,returnToEnv)
import Language.Anemone.TreeWalk.Machine (Eval,runEval,val,ctrl)
import Language.Anemone.TreeWalk.Machine (pop,push,remergePop)
import Language.Anemone.TreeWalk.Stack (StackItem(..),PushPop(..),ReturnFrom(..))
import Language.Anemone.TreeWalk.Type (typeOf,typeElim)
import Language.Anemone.TreeWalk.Value (Control(..),capture,PrimExn(..),equal)
import Language.Anemone.TreeWalk.Value (PrimCaseUnary(..),PrimCaseBin(..),PrimCaseQuat(..))
import Language.Anemone.TreeWalk.Value (PrimOp(..),PrimAp(..))
import Language.Anemone.TreeWalk.Value (PrimUnary(..),PrimBin(..))
import Language.Anemone.TreeWalk.Value (Value(..),Callable(..),toCallable,Closure(..),Laziness(..),Thunk(..))

import qualified Data.List.NonEmpty as NE
import qualified Data.List.Reverse as R
import qualified Data.Text as T
import qualified Data.Zexpr.Sexpr as Sexpr
import qualified Language.Anemone.Keywords as Keyword
import qualified Language.Anemone.TreeWalk.Environment as Env
import qualified Language.Anemone.TreeWalk.Stack as Stack
import qualified Language.Anemone.TreeWalk.Type as Ty

eval :: [Sexpr] -> Env -> IO (Either Control Value)
eval stmts env = runEval env $ case stmts of
  [] -> val NilVal
  [expr] -> elaborate expr >>= loop
  (s0:s:ss) -> do
    push $ Sequence (Sexpr.loc s0) (s:|ss)
    elaborate s0 >>= loop
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
      Nothing -> ctrl $ PrimCtrl R.nil (loc, ScopeExn valueNamespace x)
  SCombo _ Empty -> val NilVal
  opCombo@(SCombo invokedAt (SAtom _ (Sym x) :<| fs))
    | x == Keyword.operate -> case fromOperator opCombo fs of
      Right f -> do
        push $ Operate invokedAt (Sexpr.loc f) Empty
        elaborate f
      Left exn -> ctrl $ PrimCtrl R.nil exn
  SCombo invokedAt (opCombo@(SCombo _ (SAtom _ (Sym x) :<| fs)) :<| args)
    | x == Keyword.operate -> case fromOperator opCombo fs of
      Right f -> do
        push $ Operate invokedAt (Sexpr.loc f) args
        elaborate f
      Left exn -> ctrl $ PrimCtrl R.nil exn
  SCombo _ (e:<|Empty) -> elaborate e
  SCombo loc (f:<|arg:<|args) -> do
    push $ Args loc (Sexpr.loc f) (arg:|toList args)
    elaborate f
  where
  fromOperator opCombo fs = case fs of
    Empty -> Left (Sexpr.loc opCombo, SyntaxExn opCombo "expecting operative")
    f:<|Empty -> Right f
    _:<|extra:<|_ -> Left (Sexpr.loc extra, SyntaxExn extra "unexpected s-expression after operative")

reduce :: StackItem 'Pop -> Either Control Value -> Eval (Either Control Value)
reduce k (Right v) = case k of
  Operate invokedAt fLoc sexprs -> case toCallable v of
    Just (_, f) -> currentEnv >>= \inEnv -> operate k (fLoc, f) inEnv invokedAt sexprs
    Nothing -> raise PrimCtrl k (fLoc, UncallableExn v)
  Args calledAt fLoc (arg:|args) -> case toCallable v of
    Just (Strict, f) -> do
      push $ Apply calledAt (fLoc, f) (Sexpr.loc arg) args
      elaborate arg
    Just (Lazy, f) -> do
      push $ Apply calledAt (fLoc, f) (Sexpr.loc arg) args
      env0 <- currentEnv
      let mkThunk = do
            cell <- liftIO . newIORef $ Left (env0, arg)
            pure $ ThunkVal Thunk{cell,suspendedAt=Sexpr.loc arg}
      thunk <- case arg of
        SAtom _ (Sym x) -> Env.lookup env0 valueNamespace x >>= \case
          Just Bound{value} -> pure value
          Nothing -> mkThunk
        _ -> mkThunk
      val thunk
    Nothing -> raise PrimCtrl k (fLoc, UncallableExn v)
  ArgVal calledAt fLoc arg -> case toCallable v of
    Just (_, f) -> apply k calledAt f arg
    Nothing -> raise PrimCtrl k (fLoc, UncallableExn v)
  Apply1 calledAt (_, f) argLoc -> apply k calledAt f (argLoc, v)
  Restore FromThunk{cell=Thunk{cell}} env0 -> do
    liftIO $ writeIORef cell (Right v)
    returnToEnv env0
    val v
  Restore _ env0 -> do
    returnToEnv env0
    val v
  Then _ stmt -> do
    !_ <- pure $! force v
    elaborate stmt
  Cond pLoc c arcs -> case v of
    BoolVal True -> elaborate c
    BoolVal False -> case arcs of
      Empty -> val NilVal
      (p',c'):<|arcs' -> do
        push $ Cond (Sexpr.loc p') c' arcs'
        elaborate p'
    _ -> ctrl $ PrimCtrl (R.singleton $ Stack.toPush k) (pLoc, TypeError Ty.primBool v)
  OpDefine inEnv _ x _ -> do
    Env.define inEnv valueNamespace x v
    val v
  OpList vs _ Empty -> val $ ListVal (vs :|> v)
  OpList vs _ (e:<|es) -> do
    push $ OpList (vs :|> v) (Sexpr.loc e) es
    elaborate e
  PrimArg _ _ _ _ -> error "internal error: popped `PrimArg` continuation while reducing"
reduce k1 (Left exn) = do
  k <- remergePop k1
  -- TODO catch exceptions
  ctrl $ exn `capture` k

apply :: StackItem 'Pop -> Loc -> Callable -> (Loc, Value) -> Eval (Either Control Value)
apply k calledAt op@(OperPrim _) _ = raise PrimCtrl k (calledAt, UnexpectedOperative op)
apply _ calledAt (CallPrim PrimEval) a = case a of
  (envLoc, EnvVal inEnv) -> val $ PrimAp (PrimEval1 (envLoc, inEnv))
  _ -> raisePrimArg 1 calledAt PrimEval (a:|[]) (TypeError Ty.primEnv (snd a))
apply _ calledAt (CallPrim (PrimEval1 envArg@(_, evaleeEnv))) b = case b of
  (_, SexprVal evalee) -> do
    let trace = FromEval{evaledAt=calledAt,evaleeEnv,evalee}
    enterEnv trace evaleeEnv
    elaborate evalee
  _ -> raisePrimArg 2 calledAt PrimEval (second EnvVal envArg:|[b]) (TypeError Ty.primSexpr (snd b))
apply _ calledAt (CallPrim PrimDefineIn) a = case a of
  (envLoc, EnvVal inEnv) -> val $ PrimAp (PrimDefineIn3 (envLoc, inEnv))
  _ -> raisePrimArg 1 calledAt PrimDefineIn (a:|[]) (TypeError Ty.primEnv (snd a))
apply _ calledAt (CallPrim (PrimDefineIn3 inEnv)) b = case b of
  (nsLoc, SymVal ns) -> val $ PrimAp (PrimDefineIn2 inEnv (nsLoc, ns))
  _ -> raisePrimArg 2 calledAt PrimDefineIn (second EnvVal inEnv:|[b]) (TypeError Ty.primSym (snd b))
apply _ calledAt (CallPrim (PrimDefineIn2 inEnv ns)) c = case c of
  (xLoc, SymVal x) -> val $ PrimAp (PrimDefineIn1 inEnv ns (xLoc, x))
  _ -> raisePrimArg 3 calledAt PrimDefineIn (second EnvVal inEnv:|[second SymVal ns,c]) (TypeError Ty.primSym (snd c))
apply _ _ (CallPrim (PrimDefineIn1 (_, inEnv) (_, ns) (_, x))) (_, v) = do
  Env.define inEnv ns x v
  val v
apply _ forcedAt (CallPrim PrimForce) (_, v) = case v of
  ThunkVal thunk@Thunk{cell} -> liftIO (readIORef cell) >>= \case
    Left (thunkeeEnv, thunkee) -> do
      let trace = FromThunk
            { cell = thunk
            , forcedAt
            , thunkeeEnv
            , thunkee
            }
      enterEnv trace thunkeeEnv
      elaborate thunkee
    Right forced -> val forced
  _ -> val v
apply _ calledAt (CallPrim (PrimUnary prim)) a = case evalPrimUnary prim a of
  Right v -> val v
  Left exn -> raisePrimArg 1 calledAt (PrimUnary prim) (a:|[]) exn
apply _ _ (CallPrim (PrimBin prim)) a = val $ PrimAp (PrimBin1 prim a)
apply _ calledAt (CallPrim (PrimBin1 prim a)) b = case evalPrimBin prim a b of
  Right v -> val v
  Left (n, exn) -> raisePrimArg n calledAt (PrimBin prim) (a:|[b]) exn
apply _ _ (CallPrim (PrimCaseUnary prim)) a = val $ PrimAp (PrimCaseUnary1 prim a)
apply _ calledAt (CallPrim (PrimCaseUnary1 prim v)) a = evalPrimCaseUnary calledAt prim v a
apply _ _ (CallPrim (PrimCaseBin prim)) v = val $ PrimAp (PrimCaseBin2 prim v)
apply _ _ (CallPrim (PrimCaseBin2 prim v)) a = val $ PrimAp (PrimCaseBin1 prim v a)
apply _ calledAt (CallPrim (PrimCaseBin1 prim v a)) b = evalPrimCaseBin calledAt prim v a b
apply _ _ (CallPrim (PrimCaseQuat prim)) v = val $ PrimAp (PrimCaseQuat4 prim v)
apply _ _ (CallPrim (PrimCaseQuat4 prim v)) a = val $ PrimAp (PrimCaseQuat3 prim v a)
apply _ _ (CallPrim (PrimCaseQuat3 prim v a)) b = val $ PrimAp (PrimCaseQuat2 prim v a b)
apply _ _ (CallPrim (PrimCaseQuat2 prim v a b)) c = val $ PrimAp (PrimCaseQuat1 prim v a b c)
apply _ calledAt (CallPrim (PrimCaseQuat1 prim v a b c)) d = evalPrimCaseQuat calledAt prim v a b c d
apply _ calledAt (CallClosure f@Closure{scope,args,params,body}) (_, arg) = case params of
  param:|(p:ps) -> val $ ClosureVal f{args=args `snoc` (param,arg), params=p:|ps}
  param:|[] -> do
    let args' = args `snoc` (param,arg)
        params0 = case R.toList (fst <$> args) of
          [] -> params
          (a:as) -> a :| (as ++ NE.toList params)
        callee = f{args=R.nil,params=params0} :: Closure -- a version of the closure with arguments/parameters reset
    calleeEnv <- Env.newChild scope <&> \env -> env{createdAt=Just calledAt}
    forM_ args' $ \((_, x), v) -> Env.define calleeEnv valueNamespace x v
    let trace = FromCall
          { calledAt
          , callee
          }
    enterEnv trace calleeEnv
    elaborate body

applyImmediate :: Loc -> (Loc, Value) -> NonEmpty Value -> Eval (Either Control Value)
applyImmediate calledAt (fLoc, f) args = do
  push $ ArgVals calledAt fLoc ((LocUnknown,) <$> args) -- TODO perhaps I should ask for argument locations
  val f

operate :: StackItem 'Pop -> (Loc, Callable) -> Env -> Loc -> Seq Sexpr -> Eval (Either Control Value)
operate _ (_, OperPrim PrimLambda) env loc sexprs = case sexprs of
  paramSexprs:<|body:<|Empty -> case toParamList paramSexprs of
    Right params -> val $ ClosureVal Closure
      { name = Nothing
      , definedAt = loc
      , scope = env
      , args = R.nil
      , params
      , body
      }
    Left exn -> ctrl $ PrimCtrl R.nil exn
  Empty -> ctrl $ PrimCtrl R.nil (loc, SyntaxExn (SCombo loc sexprs) "expecting parameter list and function body")
  _:<|Empty -> ctrl $ PrimCtrl R.nil (loc, SyntaxExn (SCombo loc sexprs) "expecting function body")
  _:<|_:<|extra:<|_ -> ctrl $ PrimCtrl R.nil (Sexpr.loc extra, SyntaxExn extra "unexpected s-expr after function body")
  where
    -- (Sexpr.loc paramSexprs, SyntaxExn paramSexprs "expecting parameter list")
  -- TODO toParamList should specify the offending item rather than just saying the whole thing is bad
  toParamList lst@(SAtom _ _) = Left (Sexpr.loc lst, SyntaxExn lst "expecting parameter list")
  toParamList lst@(SCombo _ params) = go params
    where
    go Empty = Left (Sexpr.loc lst, SyntaxExn lst "expecting non-empty parameter list")
    go (param:<|Empty) = go1 param <&> (:|[])
    go (param:<|rest) = NE.cons <$> go1 param <*> go rest
    go1 (SAtom _ (Sym x)) = Right (Strict, x)
    go1 (SCombo _ (SAtom _ (Sym laziness) :<| SAtom _ (Sym x) :<| Empty))
      -- TODO | laziness == intern "!" = Right (Strict, x) -- actually, bang should probably force any incoming thunks automatically
      -- however, this need not be a feature of the core, but can be implemented in anemone itself
      | laziness == intern "~" = Right (Lazy, x)
    go1 nonParam = Left (Sexpr.loc nonParam, SyntaxExn nonParam "expecting parameter")
operate _ (_, OperPrim PrimSequence) _ loc sexprs = case sexprs of
  Empty -> ctrl $ PrimCtrl R.nil (loc, SyntaxExn (SCombo loc sexprs) "expecting one or more statements")
  stmt:<|Empty -> elaborate stmt
  stmt:<|s:<|ss -> do
    push $ Sequence (Sexpr.loc stmt) (s:|toList ss)
    elaborate stmt
operate _ (_, OperPrim PrimDefine) env loc sexprs = case sexprs of
  SAtom _ (Sym x) :<| body :<| Empty -> do
    push $ OpDefine env loc x (Sexpr.loc body)
    elaborate body
  Empty -> ctrl $ PrimCtrl R.nil (loc, SyntaxExn (SCombo loc sexprs) "expecting symbol")
  _:<|Empty -> ctrl $ PrimCtrl R.nil (loc, SyntaxExn (SCombo loc sexprs) "expecting definition body")
  nonSymbol:<|_:<|Empty -> ctrl $ PrimCtrl R.nil (Sexpr.loc nonSymbol, SyntaxExn nonSymbol "expecting symbol")
  _:<|_:<|extra:<|_ -> ctrl $ PrimCtrl R.nil (Sexpr.loc extra, SyntaxExn extra "unexpected s-expr after definition body")
operate _ (_, OperPrim PrimList) _ _ sexprs = case sexprs of
  Empty -> val $ ListVal Empty
  x:<|xs -> do
    push $ OpList Empty (Sexpr.loc x) xs
    elaborate x
operate _ (_, OperPrim PrimCond) _ _ sexprs = case toArc `mapM` sexprs of
  Right ((p,c):<|arcs) -> do
    push $ Cond (Sexpr.loc p) c arcs
    elaborate p
  Right Empty -> val NilVal
  Left nonArc -> ctrl $ PrimCtrl R.nil (Sexpr.loc nonArc, SyntaxExn nonArc "expecting predicate-consequent pair")
  where
  toArc (SCombo _ (p :<| c :<| Empty)) = Right (p, c)
  toArc nonArc = Left nonArc
operate k (loc, ap@(CallPrim _)) _ _ _ = raise PrimCtrl k (loc, UnexpectedApplicative ap)
operate _ (fLoc, CallClosure f) env loc sexprs =
  let args = EnvVal env :| [LocVal loc, ListVal $ SexprVal <$> sexprs]
   in applyImmediate loc (fLoc, ClosureVal f) args

raise :: (RList (StackItem 'Push) -> a -> Control) -> StackItem 'Pop -> a -> Eval (Either Control Value)
raise mk k exn = ctrl $ mk (R.singleton $ Stack.toPush k) exn

raisePrimArg :: Int -> Loc -> PrimAp -> NonEmpty (Loc, Value) -> PrimExn -> Eval (Either Control Value)
raisePrimArg n calledAt prim vs exn =
  let k = R.singleton $ PrimArg n calledAt prim vs
      (loc, _) = vs NE.!! (n - 1)
   in ctrl $ PrimCtrl k (loc, exn)












evalPrimUnary :: PrimUnary -> (Loc, Value) -> Either PrimExn Value
evalPrimUnary PrimSexprIntro (_, IntVal n) = Right . SexprVal $ SAtom LocUnknown (Int n)
evalPrimUnary PrimSexprIntro (_, StrVal s) = Right . SexprVal $ SAtom LocUnknown (Str s)
evalPrimUnary PrimSexprIntro (_, SymVal x) = Right . SexprVal $ SAtom LocUnknown (Sym x)
evalPrimUnary PrimSexprIntro (_, ListVal xs0)
  | Just sexprs <- fromSexprList xs0 = Right . SexprVal $ SCombo LocUnknown sexprs
  where
  fromSexprList Empty = Just Empty
  fromSexprList ((SexprVal sexpr) :<| xs) = (sexpr :<|) <$> fromSexprList xs
  fromSexprList _ = Nothing
evalPrimUnary PrimSexprIntro (_, v) = Left $ TypeError Ty.primSexprable v
evalPrimUnary PrimSymIntro (_, StrVal s) = Right $ SymVal (intern $ T.unpack s)
evalPrimUnary PrimSymIntro (_, v) = Left $ TypeError Ty.primStr v
evalPrimUnary PrimSymElim (_, SymVal x) = Right $ StrVal (T.pack $ unintern x)
evalPrimUnary PrimSymElim (_, v) = Left $ TypeError Ty.primSym v
evalPrimUnary PrimTypeOf (_, v) = Right $ TypeVal (typeOf v)

evalPrimBin :: PrimBin -> (Loc, Value) -> (Loc, Value) -> Either (Int, PrimExn) Value
evalPrimBin PrimEqual (_, a) (_, b) = Right $ BoolVal (a `equal` b)
evalPrimBin PrimAdd (_, IntVal a) (_, IntVal b) = Right $ IntVal (a + b)
evalPrimBin PrimAdd (_, IntVal _) (_, v) = Left (2, TypeError Ty.primInt v)
evalPrimBin PrimAdd (_, v) _ = Left (1, TypeError Ty.primInt v)
evalPrimBin PrimCons (_, x) (_, ListVal xs) = Right $ ListVal (x :<| xs)
evalPrimBin PrimCons _ (_, v) = Left (2, TypeError Ty.primList v)
evalPrimBin PrimUpdName (_, SymVal x) (_, v) = case v of
  ClosureVal f -> Right $ ClosureVal f{name = Just x}
  EnvVal e -> Right $ EnvVal e{name = Just x}
  _ -> Right v
evalPrimBin PrimUpdName (_, v) _ = Left (1, TypeError Ty.primSym v)
evalPrimBin PrimUpdLoc (_, LocVal loc) (_, v) = case v of
  SexprVal (SAtom _ atom) -> Right $ SexprVal (SAtom loc atom)
  SexprVal (SCombo _ sexprs) -> Right $ SexprVal (SCombo loc sexprs)
  ClosureVal f -> Right $ ClosureVal f{definedAt = loc}
  EnvVal e -> Right $ EnvVal e{createdAt = Just loc}
  _ -> Right v
evalPrimBin PrimUpdLoc (_, v) _ = Left (1, TypeError Ty.primLoc v)

evalPrimCaseUnary :: Loc
                  -> PrimCaseUnary -> (Loc, Value)
                  -> (Loc, Value)
                  -> Eval (Either Control Value)
evalPrimCaseUnary calledAt PrimTypeElim (_, TypeVal ty) body =
  let (tycon, args) = typeElim ty
   in applyImmediate calledAt body $ TyconVal tycon :| [ListVal $ args]
evalPrimCaseUnary calledAt PrimTypeElim v a =
  raisePrimArg 1 calledAt (PrimCaseUnary PrimTypeElim) (v:|[a]) (TypeError Ty.primType (snd v))

evalPrimCaseBin :: Loc
                -> PrimCaseBin -> (Loc, Value)
                -> (Loc, Value) -> (Loc, Value)
                -> Eval (Either Control Value)
evalPrimCaseBin calledAt PrimUncons (_, ListVal lst) onNull onCons = case lst of
  Empty -> applyImmediate calledAt onNull $ NilVal :| []
  x:<|xs -> applyImmediate calledAt onCons $ x :| [ListVal xs]
evalPrimCaseBin calledAt PrimUncons v a b =
  raisePrimArg 1 calledAt (PrimCaseBin PrimUncons) (v:|[a,b]) (TypeError Ty.primList (snd v))

evalPrimCaseQuat :: Loc
                 -> PrimCaseQuat -> (Loc, Value)
                 -> (Loc, Value) -> (Loc, Value) -> (Loc, Value) -> (Loc, Value)
                 -> Eval (Either Control Value)
evalPrimCaseQuat calledAt PrimSexprElim (_, SexprVal sexpr) onInt onStr onSym onCombo = case sexpr of
  SAtom _ (Int n) -> applyImmediate calledAt onInt $ IntVal n :| []
  SAtom _ (Str s) -> applyImmediate calledAt onStr $ StrVal s :| []
  SAtom _ (Sym x) -> applyImmediate calledAt onSym $ SymVal x :| []
  SCombo _ sexprs -> applyImmediate calledAt onCombo $ ListVal (SexprVal <$> sexprs) :| []
evalPrimCaseQuat calledAt PrimSexprElim v a b c d =
  raisePrimArg 1 calledAt (PrimCaseQuat PrimSexprElim) (v:|[a,b,c,d]) (TypeError Ty.primSexpr (snd v))
