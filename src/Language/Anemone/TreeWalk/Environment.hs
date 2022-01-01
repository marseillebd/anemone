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
  , newChild
  ) where

import Prelude hiding (lookup)

import Control.Monad.IO.Class (MonadIO,liftIO)
import Data.IORef (newIORef,readIORef,modifyIORef')
import Data.Symbol.Unsafe (Symbol(..))
import Language.Anemone.TreeWalk.Unsafe.Types (Env(..),Namespace(..),Binding(..),Name,NameCrumb(..))
import Language.Anemone.TreeWalk.Unsafe.Types (Value)

import qualified Data.IntMap.Strict as Map
import qualified Data.IntSet as Set


newEnv :: (MonadIO io) => Maybe Env -> io Env
newEnv parent = do
  nsMap <- liftIO $ newIORef Map.empty
  pure Env{parent,namespaces=nsMap,name=Nothing,createdAt=Nothing}

newEmptyEnv :: (MonadIO io) =>  io Env
newEmptyEnv = newEnv Nothing

newChild :: (MonadIO io) => Env -> io Env
newChild = newEnv . Just

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
