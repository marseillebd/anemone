{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE StandaloneDeriving #-}

module Language.Bslisp.TreeWalk.Unsafe.Types
  ( Value(..)
  -- * Functions
  , Callable(..)
  , Closure(..)
  -- * Runtime System
  , Sexpr(..)
  , Atom(..)
  , BsType(..)
  , PrimOp(..)
  , PrimAp(..)
  , PrimUnary(..)
  , PrimBin(..)
  , PrimCaseBin(..)
  , PrimCaseQuat(..)
  , PrimExn(..)
  -- * Environments
  , Env(..)
  , Namespace(..)
  , Binding(..)
  -- * Continuations
  , PushPop(..)
  , StackItem(..)
  , ReturnFrom(..)
  , StackTrace(..)
  , TraceItem(..)
  ) where

import Control.DeepSeq (NFData)
import Data.IntMap.Strict (IntMap)
import Data.IORef (IORef)
import Data.Kind (Type)
import Data.List.NonEmpty (NonEmpty)
import Data.List.Reverse (RList)
import Data.Symbol (Symbol)
import Data.Text (Text)
import Data.Zexpr.Location (Loc)
import Data.Zexpr.Sexpr (Sexpr(..),Atom(..))
import GHC.Generics (Generic)

------------------------------------ Simple Values ------------------------------------

data Value
  = NilVal
  | IntVal !Integer
  -- TODO other low-level numerical types
  | StrVal !Text
  -- TODO bytestring, (mutable) bytearray types
  | SymVal !Symbol
  -- TODO structural types
  | ListVal [Value]
  | LocVal !Loc
  | SexprVal !Sexpr
  | EnvVal !Env
  | PrimOp !PrimOp
  | PrimAp !PrimAp
  | ClosureVal !Closure
  | PrimExn !PrimExn
  -- TODO Thunk
  -- TODO ADTs/user-defined types, wrapped types (i.e. as close as I can get to Haskell newtype)
  -- TODO mutable cell and array types
  deriving (Show,Generic)
instance NFData Value

------------------------------------ Functions ------------------------------------

data Callable
  = OperPrim PrimOp
  | CallPrim PrimAp
  | CallClosure Closure
  deriving (Show,Generic)
instance NFData Callable

data Closure = Closure
  { name :: !(Maybe Symbol)
  , definedAt :: !Loc
  , scope :: !Env
  , args :: !(RList (Symbol, Value))
  , params :: !(NonEmpty Symbol)
  , body :: !Sexpr
  }
  deriving(Show,Generic)
instance NFData Closure

------------------------------------ Abstract Data Types ------------------------------------

-- TODO WrappedType
-- TODO ADT

------------------------------------ Types ------------------------------------

data BsType = BsType
  { qualName :: [Symbol] -- FIXME I should define an FQName type around lists of symbols
  -- TODO probably methods, and who knows what else
  }
  deriving (Show)

------------------------------------ Environments ------------------------------------

data Env = Env
  { parent :: !(Maybe Env)
  , namespaces :: !(IORef (IntMap Namespace))
  }
  deriving (Generic)
instance NFData Env
instance Show Env where
  show _ = "<Env>"

data Namespace = Ns
  { name :: !Symbol
  , bindings :: !(IORef (IntMap Binding))
  }
  deriving (Generic)
instance NFData Namespace

data Binding = Bound
  { name :: !Symbol
  , value :: !Value
  }
  deriving (Generic)
instance NFData Binding


------------------------------------ Primitive Applicatives/Operatives/Exceptions ------------------------------------

data PrimOp
  = PrimLambda
  | PrimSequence
  | PrimDefine
  | PrimList
  deriving (Show,Generic)
instance NFData PrimOp

data PrimAp
  = PrimEval
  | PrimEval1 Value
  | PrimDefineIn
  | PrimDefineIn3 Env
  | PrimDefineIn2 Env Symbol
  | PrimDefineIn1 Env Symbol Symbol
  | PrimUnary PrimUnary
  | PrimBin PrimBin
  | PrimBin1 PrimBin Value
  | PrimCaseBin PrimCaseBin
  | PrimCaseBin2 PrimCaseBin Value
  | PrimCaseBin1 PrimCaseBin Value Value
  | PrimCaseQuat PrimCaseQuat
  | PrimCaseQuat4 PrimCaseQuat Value
  | PrimCaseQuat3 PrimCaseQuat Value Value
  | PrimCaseQuat2 PrimCaseQuat Value Value Value
  | PrimCaseQuat1 PrimCaseQuat Value Value Value Value
  deriving (Show,Generic)
instance NFData PrimAp

data PrimUnary
  = PrimSexprIntro
  | PrimSymIntro
  | PrimSymElim
  deriving (Show,Generic)
instance NFData PrimUnary

data PrimBin
  = PrimAdd
  | PrimCons
  | PrimUpdName
  | PrimUpdLoc
  deriving (Show,Generic)
instance NFData PrimBin

data PrimCaseBin
  = PrimUncons
  deriving (Show,Generic)
instance NFData PrimCaseBin

data PrimCaseQuat
  = PrimSexprElim
  deriving (Show,Generic)
instance NFData PrimCaseQuat

data PrimExn
  = ScopeExn Loc Symbol Symbol -- first namespace then name
  | SyntaxExn Sexpr Text
  | UncallableExn Value
  | UnexpectedOperative Callable
  | UnexpectedApplicative Callable
  | TypeError Value -- TODO specify expected type
  deriving (Show,Generic)
instance NFData PrimExn

------------------------------------ Continuations ------------------------------------

data PushPop = Push | Pop
data StackItem :: PushPop -> Type where
  Operate :: Loc -- the whole combination
             {- operative -}
          -> [Sexpr]
          -> StackItem either
  Args :: Loc -- location call was made
          {- applicative -}
       -> (NonEmpty Sexpr)
       -> StackItem either
  ArgVals :: Loc -- location call was made
             {- applicative -}
          -> (NonEmpty Value)
          -> StackItem 'Push
  ArgVal :: Loc -- location call was made
            {- applicative -}
         -> Value
         -> StackItem either
  Apply :: Loc -- location call was made
        -> Callable
           {- current argument -}
        -> [Sexpr]
        -> StackItem 'Push
  Apply1 :: Loc -- location call was made
         -> Callable
            {- argument -}
         -> StackItem 'Pop
  Restore :: ReturnFrom
          -> Env
          -> StackItem either
  Sequence :: {- current statement -}
              (NonEmpty Sexpr)
           -> StackItem 'Push
  Then :: {- current statement -}
          Sexpr
       -> StackItem 'Pop
  -- supporting primitive operatives
  OpDefine :: Env
           -> Loc
           -> Symbol
              {- definition body -}
           -> StackItem either
  OpList :: RList Value
            {- current list item -}
         -> [Sexpr]
         -> StackItem either
  -- fake stack item used for rendering stack traces, but not actually evaluation
  PrimArg :: Int -> PrimAp -> NonEmpty Value -> StackItem either -- the Int is 1-index into the argument values
deriving instance Show (StackItem either)

data ReturnFrom
  = FromCall
    { calledAt :: !Loc
    , callee :: !Closure
    , args :: ![Value]
    }
  | FromEval
    { evaledAt :: !Loc
    , evaleeEnv :: !Env
    , evalee :: !Sexpr
    }
  deriving (Show)

data StackTrace = StackTrace (RList TraceItem) PrimExn
  deriving (Show)
data TraceItem
  = CallTrace
    { callerEnv :: !Env
    , calledAt :: !Loc
    , callee :: !Closure
    , args :: ![Value]
    }
  | EvalTrace
    { evalerEnv :: !Env
    , evaledAt :: !Loc
    , evaleeEnv :: !Env
    , evalee :: !Sexpr
    }
  | PrimArgTrace Int PrimAp (NonEmpty Value)
  deriving (Show)
