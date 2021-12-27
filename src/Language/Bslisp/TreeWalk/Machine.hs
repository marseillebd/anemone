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

module Language.Bslisp.TreeWalk.Machine
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
  ) where


import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Trans.State.Strict (StateT,gets,modify',state,runStateT)
import Data.Bifunctor (second)
import Data.List.NonEmpty (NonEmpty(..))
import Language.Bslisp.TreeWalk.Environment (Env)
import Language.Bslisp.TreeWalk.Stack (Stack,StackItem(..),PushPop(..),ReturnFrom(..))
import Language.Bslisp.TreeWalk.Value (Control)
import Language.Bslisp.TreeWalk.Value (Value)

import qualified Data.List.NonEmpty as NE
import qualified Language.Bslisp.TreeWalk.Stack as Stack

newtype Eval a = Eval { unEval :: StateT Machine IO a }
  deriving (Functor, Applicative, Monad, MonadIO)

runEval :: Env -> Eval a -> IO a
runEval env action = fst <$> execEval env action

execEval :: Env -> Eval a -> IO (a, Machine)
execEval env action = runStateT (unEval action) m0
  where
  m0 = M{stack=Stack.empty,env}


data Machine = M
  { stack :: !Stack
  , env :: !Env
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
remergePop (Operate loc sexprs) = pure $ Operate loc sexprs
remergePop (Args loc args) = pure $ Args loc args
remergePop (ArgVal loc v) = unsafePop >>= \case
  Just (ArgVals _ vs) -> pure $ ArgVals loc (v:|NE.toList vs)
  Just k -> push k >> pure (ArgVals  loc (v:|[]))
  Nothing -> pure $ ArgVals loc (v:|[])
remergePop (Apply1 loc f) = unsafePop >>= \case
  Just (Apply _ _ args) -> pure $ Apply loc f args
  Just k -> push k >> pure (Apply loc f [])
  Nothing -> pure $ Apply loc f []
remergePop (Restore from env) = pure $ Restore from env
remergePop (Then stmt) = unsafePop >>= \case
  Just (Sequence stmts) -> pure $ Sequence (stmt:|NE.toList stmts)
  Just k -> push k >> pure (Sequence (stmt :| []))
  Nothing -> pure $ Sequence (stmt :| [])
remergePop (OpDefine env loc x) = pure $ OpDefine env loc x
remergePop (OpList vs sexprs) = pure $ OpList vs sexprs
remergePop (PrimArg n f vs) = pure $ PrimArg n f vs



unsafePop :: Eval (Maybe (StackItem 'Push))
unsafePop = Eval $ state $ \st@M{stack} ->
  second (\stack' -> st{stack=stack'}) (Stack.unsafePop stack)

currentEnv :: Eval Env
currentEnv = Eval $ gets env

enterEnv :: ReturnFrom -> Env -> Eval ()
enterEnv trace env' = Eval $ do
  env0 <- gets env
  unEval . push $ Restore trace env0
  modify' $ \st -> st{env=env'}

returnToEnv :: Env -> Eval ()
returnToEnv env0 = Eval $ do
  modify' $ \st ->st{env=env0}
