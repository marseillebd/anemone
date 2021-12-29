{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}

module Language.Anemone.TreeWalk.Machine
  ( Eval
  , runEval
  , execEval
  , val
  , ctrl
  , push
  , pop
  , remergePop
  , currentEnv
  , enterEnv
  , returnToEnv
  , newTypeId
  ) where


import Control.Concurrent.MVar (MVar,newMVar,modifyMVar)
import Control.Monad.IO.Class (MonadIO,liftIO)
import Control.Monad.Trans.State.Strict (StateT,gets,modify',state,runStateT)
import Data.Bifunctor (second)
import Data.List.NonEmpty (NonEmpty(..))
import Data.Word (Word64)
import Language.Anemone.TreeWalk.Environment (Env)
import Language.Anemone.TreeWalk.Stack (Stack,StackItem(..),PushPop(..),ReturnFrom(..))
import Language.Anemone.TreeWalk.Value (Control)
import Language.Anemone.TreeWalk.Value (Value)

import qualified Data.List.NonEmpty as NE
import qualified Language.Anemone.TreeWalk.Stack as Stack

newtype Eval a = Eval { unEval :: StateT Machine IO a }
  deriving (Functor, Applicative, Monad, MonadIO)

runEval :: Env -> Eval a -> IO a
runEval env action = fst <$> execEval env action

execEval :: Env -> Eval a -> IO (a, Machine)
execEval env action = runStateT (unEval action) =<< m0
  where
  m0 = do
    typeIdSupply <- newMVar 1
    pure M
      { stack=Stack.empty
      , env
      , typeIdSupply
      }


data Machine = M
  { stack :: !Stack
  , env :: !Env
  , typeIdSupply :: !(MVar Word64)
  }

val :: Value -> Eval (Either Control Value)
val = pure . Right

ctrl :: Control -> Eval (Either Control Value)
ctrl = pure . Left

push :: StackItem 'Push -> Eval ()
push k = Eval $ modify' $ \st@M{stack} ->
  st{stack=Stack.push k stack}

pop :: Eval (Maybe (StackItem 'Pop))
pop = Eval $ state $ \st@M{stack} ->
  second (\stack' -> st{stack=stack'}) (Stack.pop stack)

remergePop :: StackItem 'Pop -> Eval (StackItem 'Push)
remergePop (Operate invokedAt loc sexprs) = pure $ Operate invokedAt loc sexprs
remergePop (Args loc fLoc args) = pure $ Args loc fLoc args
remergePop (ArgVal loc fLoc v) = unsafePop >>= \case
  Just (ArgVals _ _ vs) -> pure $ ArgVals loc fLoc (v:|NE.toList vs)
  Just k -> push k >> pure (ArgVals loc fLoc (v:|[]))
  Nothing -> pure $ ArgVals loc fLoc (v:|[])
remergePop (Apply1 loc f argLoc) = unsafePop >>= \case
  Just (Apply _ _ _ args) -> pure $ Apply loc f argLoc args
  Just k -> push k >> pure (Apply loc f argLoc [])
  Nothing -> pure $ Apply loc f argLoc []
remergePop (Restore from env) = pure $ Restore from env
remergePop (Then stmtLoc stmt) = unsafePop >>= \case
  Just (Sequence _ stmts) -> pure $ Sequence stmtLoc (stmt:|NE.toList stmts)
  Just k -> push k >> pure (Sequence stmtLoc (stmt :| []))
  Nothing -> pure $ Sequence stmtLoc (stmt :| [])
remergePop (Cond pLoc c arcs) = pure $ Cond pLoc c arcs
remergePop (OpDefine env loc x bodyLoc) = pure $ OpDefine env loc x bodyLoc
remergePop (OpList vs itemLoc sexprs) = pure $ OpList vs itemLoc sexprs
remergePop (PrimArg n calledAt f vs) = pure $ PrimArg n calledAt f vs



unsafePop :: Eval (Maybe (StackItem 'Push))
unsafePop = Eval $ state $ \st@M{stack} ->
  second (\stack' -> st{stack=stack'}) (Stack.unsafePop stack)

currentEnv :: Eval Env
currentEnv = Eval $ gets env

enterEnv :: ReturnFrom -> Env -> Eval ()
enterEnv trace env' = Eval $ do
  -- FIXME tail call optimization
  env0 <- gets env
  unEval . push $ Restore trace env0
  modify' $ \st -> st{env=env'}

returnToEnv :: Env -> Eval ()
returnToEnv env0 = Eval $ do
  modify' $ \st ->st{env=env0}

newTypeId :: Eval Word64
newTypeId = Eval $ do
  supply <- gets typeIdSupply
  liftIO . modifyMVar supply $ \n -> pure (n + 1, n)
