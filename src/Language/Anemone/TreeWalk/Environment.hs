{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}

module Language.Anemone.TreeWalk.Environment
  ( Env(..)
  , Binding(..)
  , Name
  , NameCrumb(..)
  , lookup
  , define
  , newEmptyEnv
  , newDefaultEnv
  , newChild
  ) where

import Prelude hiding (lookup)

import Control.Monad (forM_)
import Control.Monad.IO.Class (MonadIO,liftIO)
import Data.IORef (newIORef,readIORef,modifyIORef')
import Data.Symbol.Unsafe (Symbol(..),intern)
import Language.Anemone.Keywords (valueNamespace)
import Language.Anemone.TreeWalk.Unsafe.Types (Env(..),Namespace(..),Binding(..),Name,NameCrumb(..))
import Language.Anemone.TreeWalk.Value (PrimCaseUnary(..),PrimCaseBin(..),PrimCaseQuat(..))
import Language.Anemone.TreeWalk.Value (PrimUnary(..),PrimBin(..))
import Language.Anemone.TreeWalk.Value (Value(..),PrimOp(..),PrimAp(..))

import qualified Data.IntMap.Strict as Map
import qualified Data.IntSet as Set
import qualified Language.Anemone.TreeWalk.Type as Type


newEnv :: (MonadIO io) => Maybe Env -> io Env
newEnv parent = do
  nsMap <- liftIO $ newIORef Map.empty
  pure Env{parent,namespaces=nsMap,name=Nothing,createdAt=Nothing}

newEmptyEnv :: (MonadIO io) =>  io Env
newEmptyEnv = newEnv Nothing

newChild :: (MonadIO io) => Env -> io Env
newChild = newEnv . Just

newDefaultEnv :: (MonadIO io) =>  io Env
newDefaultEnv = do
  env <- newEmptyEnv
  forM_ primInfo $ \(x, prim) ->
    unsafeDefine env valueNamespace (intern x) prim
  return env
  where
  primInfo =
    -- core features
    [ ("__lambda__", PrimOp PrimLambda)
    , ("__eval__", PrimAp PrimEval)
    , ("__force__", PrimAp PrimForce)
    -- sequential programming
    , ("__sequence__", PrimOp PrimSequence)
    , ("__defineHere__", PrimOp PrimDefineHere)
    -- booleans
    , ("__true__", BoolVal True)
    , ("__false__", BoolVal False)
    , ("__cond__", PrimOp PrimCond)
    , ("__equal__", PrimAp $ PrimBin PrimEqual)
    -- arithmetic
    , ("__add__", PrimAp $ PrimBin PrimAdd)
    , ("__sub__", PrimAp $ PrimBin PrimSub)
    -- lists
    , ("__list__", PrimOp PrimList)
    -- TODO length, index
    , ("__cons__", PrimAp $ PrimBin PrimCons)
    , ("__uncons__", PrimAp $ PrimCaseBin PrimUncons)
    -- TODO snoc, unsnoc
    -- TODO cat, split
    -- sexprs
    , ("__sexpr-intro__", PrimAp $ PrimUnary PrimSexprIntro)
    , ("__sexpr-elim__", PrimAp $ PrimCaseQuat PrimSexprElim)
    , ("__sym-intro__", PrimAp $ PrimUnary PrimSymIntro)
    , ("__sym-elim__", PrimAp $ PrimUnary PrimSymElim)
    -- types
    , ("__typeof__", PrimAp $ PrimUnary PrimTypeOf)
    , ("__type-elim__", PrimAp $ PrimCaseUnary PrimTypeElim)
    , ("__tycon-nil__", TypeVal Type.primNil)
    , ("__tycon-int__", TypeVal Type.primInt)
    -- environments
    , ("__new-env!__", PrimAp $ PrimUnary PrimNewEnv)
    , ("__new-emptyEnv!__", PrimAp $ PrimUnary PrimNewEmptyEnv)
    , ("__lookup__", PrimAp PrimLookup)
    , ("__define__", PrimAp PrimDefine)
    -- first-class control
    , ("__raise__", PrimAp $ PrimUnary PrimRaise) -- TODO though a primitive raise likely also needs a continuation for re-raising/continuing a raise
    , ("__syntaxErr-intro__", PrimAp $ PrimBin PrimSyntaxErrIntro)
    -- metadata
    , ("__name-intro__", PrimAp $ PrimUnary PrimNameIntro)
    , ("__name-elim__", PrimAp $ PrimCaseBin PrimNameElim)
    , ("__upd-name__", PrimAp $ PrimBin PrimUpdName)
    , ("__upd-loc__", PrimAp $ PrimBin PrimUpdLoc)
    ]

lookup :: (MonadIO io) => Env -> Symbol -> Symbol -> io (Maybe Binding)
lookup env0 (Symbol nsId _) (Symbol xId _) = liftIO $ go env0
  where
  go env = Map.lookup nsId <$> readIORef (namespaces env) >>= \case
    Just ns -> do
      Map.lookup xId <$> readIORef (bindings ns) >>= \case
        Just bound -> pure (Just bound)
        Nothing -> goParent env
    Nothing -> goParent env
  goParent env = case parent env of
    Nothing -> pure Nothing
    Just p -> go p

-- return True on success
define :: (MonadIO io) => Env -> Symbol -> Symbol -> Value -> io Bool
define env ns x@(Symbol xId _) v = do
  Ns{bindings,reserved} <- ensureNamespace env ns
  inBindings <- Map.lookup xId <$> liftIO (readIORef bindings)
  inReserved <- Set.member xId <$> liftIO (readIORef reserved)
  case (inBindings, inReserved) of
    (Nothing, False) -> do
      reserveSuccess <- reserve (parent env) ns x
      if reserveSuccess
      then do
        liftIO $ modifyIORef' bindings $ Map.insert xId Bound{name=x,value=v}
        pure True
      else pure False
    (_, _) -> pure False

-- reserve the name in this namespace and all parents
-- return True on success
reserve :: (MonadIO io) => Maybe Env -> Symbol -> Symbol -> io Bool
reserve Nothing _ _ = pure True
reserve (Just env0) ns (Symbol xId _) = loop env0
  where
  loop env = do
    Ns{bindings,reserved} <- ensureNamespace env ns
    inBindings <- Map.lookup xId <$> liftIO (readIORef bindings)
    inReserved <- Set.member xId <$> liftIO (readIORef reserved)
    case (inBindings, inReserved) of
      (Just _, _) -> pure False
      -- if it was reserved in this env, then it was reserved in the parents, and therefore not bound in the parents either
      (Nothing, True) -> pure True
      (Nothing, False) -> do
        reserveSuccess <- maybe (pure True) loop (parent env)
        if reserveSuccess
        then do
          liftIO $ modifyIORef' reserved $ Set.insert xId
          pure True
        else pure False

ensureNamespace :: (MonadIO io) => Env -> Symbol -> io Namespace
ensureNamespace env ns@(Symbol nsId _) =
  Map.lookup nsId <$> liftIO (readIORef $ namespaces env) >>= \case
    Just namespace -> pure namespace
    Nothing -> liftIO $ do
      bindings <- newIORef Map.empty
      reserved <- newIORef Set.empty
      let namespace = Ns{name=ns,bindings,reserved}
      modifyIORef' (namespaces env) $ Map.insert nsId namespace
      pure namespace

unsafeDefine :: (MonadIO io) => Env -> Symbol -> Symbol -> Value -> io ()
unsafeDefine env ns x@(Symbol xId _) v = liftIO $ do
  bind =<< ensureNamespace env ns
  where
  bind namespace = liftIO $ modifyIORef' (bindings namespace) $
    Map.insert xId Bound{name=x,value=v}
