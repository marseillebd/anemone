module Language.Anemone.TreeWalk.Environment.Default
  ( newDefaultEnv
  ) where

import Control.Monad (forM_)
import Control.Monad.IO.Class (MonadIO)
import Data.Symbol (intern)
import Language.Anemone.Keywords (valueNamespace)
import Language.Anemone.TreeWalk.Environment (Env,newEmptyEnv,define)
import Language.Anemone.TreeWalk.Value (PrimCaseUnary(..),PrimCaseBin(..),PrimCaseQuat(..))
import Language.Anemone.TreeWalk.Value (PrimUnary(..),PrimBin(..))
import Language.Anemone.TreeWalk.Value (Value(..),PrimOp(..),PrimAp(..))

import qualified Language.Anemone.TreeWalk.Type as Type


newDefaultEnv :: (MonadIO io) =>  io Env
newDefaultEnv = do
  env <- newEmptyEnv
  forM_ primInfo $ \(x, prim) ->
    define env valueNamespace (intern x) prim
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
    , ("__tycon-unit__", TypeVal Type.primUnit)
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
