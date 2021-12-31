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

import Prelude hiding (lookup)

import Control.DeepSeq (force)
import Control.Monad (forM_,join)
import Control.Monad.IO.Class (MonadIO,liftIO)
import Data.Bifunctor (second)
import Data.Foldable (toList)
import Data.Functor ((<&>))
import Data.IORef (newIORef,readIORef,writeIORef)
import Data.List.NonEmpty (NonEmpty(..))
import Data.List.Reverse (RList,snoc)
import Data.Sequence (Seq(..))
import Data.Symbol (Symbol,intern,unintern)
import Data.Zexpr.Location (Loc(..))
import Data.Zexpr.Sexpr (Atom(..),Sexpr(..))
import Language.Anemone.TreeWalk.Environment (Env(..),Binding(..),NameCrumb(..))
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
    lookup loc env0 Keyword.valueNamespace x
  SCombo _ Empty -> val NilVal
  combo@(SCombo invokedAt (SAtom _ (Sym operate_m) :<| args0))
    | operate_m == Keyword.operate -> case args0 of
      op :<| args -> do
        push $ Operate combo (Sexpr.loc op) args
        elaborate op
      Empty -> ctrl $ PrimCtrl R.nil (invokedAt, SyntaxErr combo "expecting operator")
  SCombo _ (e:<|Empty) -> elaborate e
  SCombo loc (f:<|arg:<|args) -> do
    push $ Args loc (Sexpr.loc f) (arg:|toList args)
    elaborate f

lookup :: Loc -> Env -> Symbol -> Symbol -> Eval (Either Control Value)
lookup varAt env namespace name = do
  Env.lookup env namespace name >>= \case
    Just Bound{value} -> val value
    Nothing -> ctrl $ PrimCtrl R.nil (varAt, ScopeErr env $ NameCrumb{namespace,name} :| [])

reduce :: StackItem 'Pop -> Either Control Value -> Eval (Either Control Value)
reduce k (Right v) = case k of
  Operate fullSexpr fLoc sexprs -> case toCallable v of
    Just (_, f) -> currentEnv >>= \inEnv -> operate fullSexpr f inEnv (Sexpr.loc fullSexpr) sexprs
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
        SAtom _ (Sym x) -> Env.lookup env0 Keyword.valueNamespace x >>= \case
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
    _ -> ctrl $ PrimCtrl (R.singleton $ Stack.toPush k) (pLoc, TypeErr Ty.primBool v)
  OpDefineHere inEnv _ x _ ->
    Env.define inEnv Keyword.valueNamespace x v >>= \case
      True -> val v
      False -> error $ "unimplemented: redefinition (or shadowing) error " ++ show x -- TODO
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
apply k calledAt (OperPrim op) a = case a of
  (loc, EnvVal withEnv) -> val $ PrimAp $ PrimApplyOp2 op (loc, withEnv)
  _ -> raise PrimCtrl k (calledAt, TypeErr Ty.primEnv (snd a))
apply k calledAt (CallPrim (PrimApplyOp2 op withEnv)) b = case b of
  (loc, LocVal atLoc) -> val $ PrimAp $ PrimApplyOp1 op withEnv (loc, atLoc)
  _ -> raise PrimCtrl k (calledAt, TypeErr Ty.primType (snd b))
apply k calledAt (CallPrim (PrimApplyOp1 op withEnv inLoc)) c = case c of
  (_, ListVal vs) | Just sexprs <- fromSexprList vs -> do
    let fullSexpr = SCombo LocUnknown sexprs
    operate fullSexpr (OperPrim op) (snd withEnv) (snd inLoc) sexprs
  _ -> raise PrimCtrl k (calledAt, TypeErr Ty.primList (snd c)) -- TODO should be a list of sexprs, not just an any ist
apply _ calledAt (CallPrim PrimEval) a = case a of
  (envLoc, EnvVal inEnv) -> val $ PrimAp (PrimEval1 (envLoc, inEnv))
  _ -> raisePrimArg 1 calledAt PrimEval (a:|[]) (TypeErr Ty.primEnv (snd a))
apply _ calledAt (CallPrim (PrimEval1 envArg@(_, evaleeEnv))) b = case b of
  (_, SexprVal evalee) -> do
    let trace = FromEval{evaledAt=calledAt,evaleeEnv,evalee}
    enterEnv trace evaleeEnv
    elaborate evalee
  _ -> raisePrimArg 2 calledAt PrimEval (second EnvVal envArg:|[b]) (TypeErr Ty.primSexpr (snd b))
apply _ calledAt (CallPrim PrimLookup) a = case a of
  (envLoc, EnvVal inEnv) -> val $ PrimAp (PrimLookup2 (envLoc, inEnv))
  _ -> raisePrimArg 1 calledAt PrimLookup (a:|[]) (TypeErr Ty.primEnv (snd a))
apply _ calledAt (CallPrim (PrimLookup2 inEnv)) b = case b of
  (nsLoc, SymVal ns) -> val $ PrimAp (PrimLookup1 inEnv (nsLoc, ns))
  _ -> raisePrimArg 2 calledAt PrimLookup (second EnvVal inEnv:|[b]) (TypeErr Ty.primSym (snd b))
apply _ calledAt (CallPrim (PrimLookup1 inEnv ns)) c = case c of
  (_, SymVal x) -> lookup calledAt (snd inEnv) (snd ns) x
  _ -> raisePrimArg 3 calledAt PrimLookup (second EnvVal inEnv:|[second SymVal ns,c]) (TypeErr Ty.primSym (snd c))
apply _ calledAt (CallPrim PrimDefine) a = case a of
  (envLoc, EnvVal inEnv) -> val $ PrimAp (PrimDefine3 (envLoc, inEnv))
  _ -> raisePrimArg 1 calledAt PrimDefine (a:|[]) (TypeErr Ty.primEnv (snd a))
apply _ calledAt (CallPrim (PrimDefine3 inEnv)) b = case b of
  (nsLoc, SymVal ns) -> val $ PrimAp (PrimDefine2 inEnv (nsLoc, ns))
  _ -> raisePrimArg 2 calledAt PrimDefine (second EnvVal inEnv:|[b]) (TypeErr Ty.primSym (snd b))
apply _ calledAt (CallPrim (PrimDefine2 inEnv ns)) c = case c of
  (xLoc, SymVal x) -> val $ PrimAp (PrimDefine1 inEnv ns (xLoc, x))
  _ -> raisePrimArg 3 calledAt PrimDefine (second EnvVal inEnv:|[second SymVal ns,c]) (TypeErr Ty.primSym (snd c))
apply _ _ (CallPrim (PrimDefine1 (_, inEnv) (_, ns) (_, x))) (_, v) =
  Env.define inEnv ns x v >>= \case
    True -> val v
    False -> error $ "unimplemented: redefinition (or shadowing) error " ++ show x -- TODO
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
apply _ calledAt (CallPrim (PrimUnary prim)) a = evalPrimUnary prim a >>= \case
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
    forM_ args' $ \((_, x), v) -> Env.define calleeEnv Keyword.valueNamespace x v
    let trace = FromCall
          { calledAt
          , callee
          }
    enterEnv trace calleeEnv
    elaborate body

-- TODO perhaps there should be a version of this that takes a known callable instead of a (Loc, Value)
applyImmediate :: Loc -> (Loc, Value) -> NonEmpty Value -> Eval (Either Control Value)
applyImmediate calledAt (fLoc, f) args = do
  push $ ArgVals calledAt fLoc ((LocUnknown,) <$> args) -- TODO perhaps I should ask for argument locations
  val f

operate :: Sexpr -> Callable -> Env -> Loc -> Seq Sexpr -> Eval (Either Control Value)
operate fullSexpr (OperPrim PrimLambda) env loc sexprs = case sexprs of
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
  Empty -> ctrl $ PrimCtrl R.nil (loc, SyntaxErr fullSexpr "expecting parameter list and function body")
  _:<|Empty -> ctrl $ PrimCtrl R.nil (loc, SyntaxErr fullSexpr "expecting function body")
  _:<|_:<|extra:<|_ -> ctrl $ PrimCtrl R.nil (Sexpr.loc extra, SyntaxErr extra "unexpected s-expr after function body")
  where
  toParamList lst@(SAtom _ _) = Left (Sexpr.loc lst, SyntaxErr lst "expecting parameter list")
  toParamList lst@(SCombo _ params) = go params
    where
    go Empty = Left (Sexpr.loc lst, SyntaxErr lst "expecting non-empty parameter list")
    go (param:<|Empty) = go1 param <&> (:|[])
    go (param:<|rest) = NE.cons <$> go1 param <*> go rest
    go1 (SAtom _ (Sym x)) = Right (Strict, x)
    go1 (SCombo _ (SAtom _ (Sym laziness) :<| SAtom _ (Sym x) :<| Empty))
      -- TODO | laziness == intern "!" = Right (Strict, x) -- actually, bang should probably force any incoming thunks automatically
      -- however, this need not be a feature of the core, but can be implemented in anemone itself
      | laziness == intern "~" = Right (Lazy, x)
    go1 nonParam = Left (Sexpr.loc nonParam, SyntaxErr nonParam "expecting parameter")
operate _ (OperPrim PrimSequence) _ _ sexprs = case sexprs of
  Empty -> val NilVal
  stmt:<|Empty -> elaborate stmt
  stmt:<|s:<|ss -> do
    push $ Sequence (Sexpr.loc stmt) (s:|toList ss)
    elaborate stmt
operate fullSexpr (OperPrim PrimDefineHere) env loc sexprs = case sexprs of
  SAtom _ (Sym x) :<| body :<| Empty -> do
    push $ OpDefineHere env loc x (Sexpr.loc body)
    elaborate body
  Empty -> ctrl $ PrimCtrl R.nil (loc, SyntaxErr fullSexpr "expecting symbol")
  _:<|Empty -> ctrl $ PrimCtrl R.nil (loc, SyntaxErr fullSexpr "expecting definition body")
  nonSymbol:<|_:<|Empty -> ctrl $ PrimCtrl R.nil (Sexpr.loc nonSymbol, SyntaxErr nonSymbol "expecting symbol")
  _:<|_:<|extra:<|_ -> ctrl $ PrimCtrl R.nil (Sexpr.loc extra, SyntaxErr extra "unexpected s-expr after definition body")
operate _ (OperPrim PrimList) _ _ sexprs = case sexprs of
  Empty -> val $ ListVal Empty
  x:<|xs -> do
    push $ OpList Empty (Sexpr.loc x) xs
    elaborate x
operate _ (OperPrim PrimCond) _ _ sexprs = case toArc `mapM` sexprs of
  Right ((p,c):<|arcs) -> do
    push $ Cond (Sexpr.loc p) c arcs
    elaborate p
  Right Empty -> val NilVal
  Left nonArc -> ctrl $ PrimCtrl R.nil (Sexpr.loc nonArc, SyntaxErr nonArc "expecting predicate-consequent pair")
  where
  toArc (SCombo _ (p :<| c :<| Empty)) = Right (p, c)
  toArc nonArc = Left nonArc
operate _ (CallPrim ap) env loc sexprs =
  let args = EnvVal env :| [LocVal loc, ListVal $ SexprVal <$> sexprs]
   in applyImmediate loc (LocUnknown, PrimAp ap) args
operate _ (CallClosure f) env loc sexprs =
  let args = EnvVal env :| [LocVal loc, ListVal $ SexprVal <$> sexprs]
   in applyImmediate loc (LocUnknown, ClosureVal f) args

raise :: (RList (StackItem 'Push) -> a -> Control) -> StackItem 'Pop -> a -> Eval (Either Control Value)
raise mk k exn = ctrl $ mk (R.singleton $ Stack.toPush k) exn

raisePrimArg :: Int -> Loc -> PrimAp -> NonEmpty (Loc, Value) -> PrimExn -> Eval (Either Control Value)
raisePrimArg n calledAt prim vs exn =
  let k = R.singleton $ PrimArg n calledAt prim vs
      (loc, _) = vs NE.!! (n - 1)
   in ctrl $ PrimCtrl k (loc, exn)









fromSexprList :: Seq Value -> Maybe (Seq Sexpr)
fromSexprList Empty = Just Empty
fromSexprList ((SexprVal sexpr) :<| xs) = (sexpr :<|) <$> fromSexprList xs
fromSexprList _ = Nothing


evalPrimUnary :: (MonadIO io) => PrimUnary -> (Loc, Value) -> io (Either PrimExn Value)
evalPrimUnary PrimSexprIntro (_, IntVal n) = pure . Right . SexprVal $ SAtom LocUnknown (Int n)
evalPrimUnary PrimSexprIntro (_, StrVal s) = pure . Right . SexprVal $ SAtom LocUnknown (Str s)
evalPrimUnary PrimSexprIntro (_, SymVal x) = pure . Right . SexprVal $ SAtom LocUnknown (Sym x)
evalPrimUnary PrimSexprIntro (_, ListVal xs0)
  | Just sexprs <- fromSexprList xs0 = pure . Right . SexprVal $ SCombo LocUnknown sexprs
evalPrimUnary PrimSexprIntro (_, v) = pure . Left $ TypeErr Ty.primSexprable v
evalPrimUnary PrimSymIntro (_, StrVal s) = pure . Right $ SymVal (intern $ T.unpack s)
evalPrimUnary PrimSymIntro (_, v) = pure . Left $ TypeErr Ty.primStr v
evalPrimUnary PrimSymElim (_, SymVal x) = pure . Right $ StrVal (T.pack $ unintern x)
evalPrimUnary PrimSymElim (_, v) = pure . Left $ TypeErr Ty.primSym v
evalPrimUnary PrimTypeOf (_, v) = pure . Right $ TypeVal (typeOf v)
evalPrimUnary PrimNewEnv (_, EnvVal parent) = Right . EnvVal <$> Env.newChild parent
evalPrimUnary PrimNewEnv (_, v) = pure . Left $ TypeErr Ty.primEnv v
evalPrimUnary PrimNewEmptyEnv (_, NilVal) = Right . EnvVal <$> Env.newEmptyEnv
evalPrimUnary PrimNewEmptyEnv (_, v) = pure . Left $ TypeErr Ty.primEnv v
evalPrimUnary PrimRaise (_, PrimExn exn) = pure . Left $ exn
evalPrimUnary PrimRaise (_, v) = pure . Left $ TypeErr Ty.primPrompt v
evalPrimUnary PrimNameIntro (loc, SymVal ns) = pure . Right $ PrimAp $ PrimBin1 PrimNameIntro1 (loc, SymVal ns)
evalPrimUnary PrimNameIntro (_, ListVal lst)
  | Just crumbs <- fromNameList lst = pure . Right $ NameVal (join crumbs)
  where
  fromNameList (NameVal n :<| Empty) = Just (n :| [])
  fromNameList (NameVal n :<| rest) = (n `NE.cons`) <$> fromNameList rest
  fromNameList _ = Nothing
evalPrimUnary PrimNameIntro (_, v) = pure . Left $ TypeErr Ty.primNameIntroable v

evalPrimBin :: PrimBin -> (Loc, Value) -> (Loc, Value) -> Either (Int, PrimExn) Value
evalPrimBin PrimEqual (_, a) (_, b) = Right $ BoolVal (a `equal` b)
evalPrimBin PrimAdd (_, IntVal a) (_, IntVal b) = Right $ IntVal (a + b)
evalPrimBin PrimAdd (_, IntVal _) (_, v) = Left (2, TypeErr Ty.primInt v)
evalPrimBin PrimAdd (_, v) _ = Left (1, TypeErr Ty.primInt v)
evalPrimBin PrimSub (_, IntVal a) (_, IntVal b) = Right $ IntVal (a - b)
evalPrimBin PrimSub (_, IntVal _) (_, v) = Left (2, TypeErr Ty.primInt v)
evalPrimBin PrimSub (_, v) _ = Left (1, TypeErr Ty.primInt v)
evalPrimBin PrimCons (_, x) (_, ListVal xs) = Right $ ListVal (x :<| xs)
evalPrimBin PrimCons _ (_, v) = Left (2, TypeErr Ty.primList v)
evalPrimBin PrimSyntaxErrIntro (_, SexprVal sexpr) (_, StrVal msg) =
  Right . PrimExn $ SyntaxErr sexpr msg
evalPrimBin PrimSyntaxErrIntro (_, SexprVal _) (_, b) = Left (2, TypeErr Ty.primStr b)
evalPrimBin PrimSyntaxErrIntro (_, a) _ = Left (1, TypeErr Ty.primSexpr a)
evalPrimBin PrimUpdName (_, NameVal x) (_, v) = case v of
  ClosureVal f -> Right $ ClosureVal f{name = Just x}
  EnvVal e -> Right $ EnvVal e{name = Just x}
  _ -> Right v
evalPrimBin PrimUpdName (_, v) _ = Left (1, TypeErr Ty.primName v)
evalPrimBin PrimUpdLoc (_, LocVal loc) (_, v) = case v of
  SexprVal (SAtom _ atom) -> Right $ SexprVal (SAtom loc atom)
  SexprVal (SCombo _ sexprs) -> Right $ SexprVal (SCombo loc sexprs)
  ClosureVal f -> Right $ ClosureVal f{definedAt = loc}
  EnvVal e -> Right $ EnvVal e{createdAt = Just loc}
  _ -> Right v
evalPrimBin PrimUpdLoc (_, v) _ = Left (1, TypeErr Ty.primLoc v)
evalPrimBin PrimNameIntro1 (_, (SymVal namespace)) (_, SymVal name) =
  Right $ NameVal (NameCrumb{namespace,name} :| [])
evalPrimBin PrimNameIntro1 (_, (SymVal _)) (_, v) = Left (2, TypeErr Ty.primSym v)
evalPrimBin PrimNameIntro1 _ _ = errorWithoutStackTrace "internal error: non-symbol saved as first argument to PrimNameIntro1"

evalPrimCaseUnary :: Loc
                  -> PrimCaseUnary -> (Loc, Value)
                  -> (Loc, Value)
                  -> Eval (Either Control Value)
evalPrimCaseUnary calledAt PrimTypeElim (_, TypeVal ty) body =
  let (tycon, args) = typeElim ty
   in applyImmediate calledAt body $ TyconVal tycon :| [ListVal $ args]
evalPrimCaseUnary calledAt PrimTypeElim v a =
  raisePrimArg 1 calledAt (PrimCaseUnary PrimTypeElim) (v:|[a]) (TypeErr Ty.primType (snd v))

evalPrimCaseBin :: Loc
                -> PrimCaseBin -> (Loc, Value)
                -> (Loc, Value) -> (Loc, Value)
                -> Eval (Either Control Value)
evalPrimCaseBin calledAt PrimUncons (_, ListVal lst) onNull onCons = case lst of
  Empty -> applyImmediate calledAt onNull $ NilVal :| []
  x:<|xs -> applyImmediate calledAt onCons $ x :| [ListVal xs]
evalPrimCaseBin calledAt PrimUncons v a b =
  raisePrimArg 1 calledAt (PrimCaseBin PrimUncons) (v:|[a,b]) (TypeErr Ty.primList (snd v))
evalPrimCaseBin calledAt PrimNameElim (_, NameVal crumbs) onQualified onFinal = case crumbs of
  NameCrumb{namespace,name}:|crumbs' -> case crumbs' of
    (c:cs) -> applyImmediate calledAt onQualified $ SymVal namespace :| [SymVal name, NameVal (c:|cs)]
    [] -> applyImmediate calledAt onFinal $ SymVal namespace :| [SymVal name]
evalPrimCaseBin calledAt PrimNameElim v a b =
  raisePrimArg 1 calledAt (PrimCaseBin PrimNameElim) (v:|[a,b]) (TypeErr Ty.primList (snd v))

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
  raisePrimArg 1 calledAt (PrimCaseQuat PrimSexprElim) (v:|[a,b,c,d]) (TypeErr Ty.primSexpr (snd v))
