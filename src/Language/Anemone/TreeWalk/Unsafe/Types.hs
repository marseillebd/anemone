{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE StandaloneDeriving #-}

module Language.Anemone.TreeWalk.Unsafe.Types
  ( Value(..)
  -- * Functions
  , Callable(..)
  , Closure(..)
  , Laziness(..)
  -- * Runtime System
  , Sexpr(..)
  , Atom(..)
  , AType(..)
  , ATypeInfo(..)
  , Tycon(..)
  , PrimType(..)
  , PrimOp(..)
  , PrimAp(..)
  , PrimUnary(..)
  , PrimBin(..)
  , PrimCaseUnary(..)
  , PrimCaseBin(..)
  , PrimCaseQuat(..)
  , PrimExn(..)
  -- * Environments
  , Env(..)
  , Namespace(..)
  , Binding(..)
  , Name
  , NameCrumb(..)
  -- * Thunks
  , Thunk(..)
  -- * Continuations
  , PushPop(..)
  , StackItem(..)
  , ReturnFrom(..)
  , StackTrace(..)
  , TraceItem(..)
  ) where

import Control.DeepSeq (NFData)
import Data.IntMap.Strict (IntMap)
import Data.IntSet (IntSet)
import Data.IORef (IORef)
import Data.Kind (Type)
import Data.List.NonEmpty (NonEmpty)
import Data.List.Reverse (RList)
import Data.Sequence (Seq)
import Data.Symbol (Symbol)
import Data.Text (Text)
import Data.Zexpr.Location (Loc)
import Data.Zexpr.Sexpr (Sexpr(..),Atom(..))
import GHC.Generics (Generic)

------------------------------------ Simple Values ------------------------------------

data Value
  = NilVal
  | BoolVal !Bool
  | IntVal !Integer
  -- TODO other low-level numerical types
  | StrVal !Text
  -- TODO bytestring, (mutable) bytearray types
  | SymVal !Symbol
  -- TODO structural types
  | ListVal !(Seq Value)
  | SexprVal !Sexpr
  | TypeVal !AType
  | TyconVal !Tycon
  | EnvVal !Env
  | PrimOp !PrimOp
  | PrimAp !PrimAp
  | ClosureVal !Closure
  | ThunkVal !Thunk
  | PrimExn !PrimExn
  | NameVal !Name
  | LocVal !Loc
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
  { name :: !(Maybe Name)
  , definedAt :: !Loc
  , scope :: !Env
  , args :: !(RList ((Laziness, Symbol), Value))
  , params :: !(NonEmpty (Laziness, Symbol))
  , body :: !Sexpr
  }
  deriving(Show,Generic)
instance NFData Closure

data Laziness = Strict | Lazy
  deriving(Show,Generic)
instance NFData Laziness

------------------------------------ Abstract Data Types ------------------------------------

-- TODO WrappedType
-- TODO ADT

------------------------------------ Types ------------------------------------

data AType = AType
  { info :: !ATypeInfo -- this determines the identity of a type
  -- , name :: !Name -- FIXME I should define an FQName type around lists of symbols
  -- , definedAt :: !Loc -- TODO
  }
  deriving (Show,Generic)
instance NFData AType

data ATypeInfo
  = PrimType !PrimType
  | UnionTy (Seq AType)
  -- TODO quantified types, type variables
  -- TODO more structural types
  -- TODO | UserType -- TODO I need a lot more inf ohere (a unique id, callability, parameters/arguments, fields, &c)
  deriving (Show,Generic)
instance NFData ATypeInfo

data Tycon
  = PrimTycon !PrimType
  | UnionTycon
  -- TODO user-defined type constructors
  deriving (Eq,Show,Generic)
instance NFData Tycon

------------------------------------ Thunks ------------------------------------

data Thunk = Thunk
  { cell :: !(IORef (Either (Env, Sexpr) Value))
  , suspendedAt :: !Loc
  }
  deriving(Generic)
instance NFData Thunk
instance Show Thunk where
  show _ = "<Thunk>"

------------------------------------ Environments ------------------------------------

data Env = Env
  { parent :: !(Maybe Env)
  , namespaces :: !(IORef (IntMap Namespace))
  , name :: !(Maybe Name)
  , createdAt :: !(Maybe Loc)
  }
  deriving (Generic)
instance NFData Env
instance Show Env where
  show Env{name} = "<Env " ++ show name ++ ">"

data Namespace = Ns
  { name :: !Symbol
  , bindings :: !(IORef (IntMap Binding))
  , reserved :: !(IORef IntSet)
  }
  deriving (Generic)
instance NFData Namespace

data Binding = Bound
  { name :: !Symbol
  , value :: !Value
  }
  deriving (Generic)
instance NFData Binding

-- basically a list of namespace/variable pairs, each subsequent pair is looked up in the environment bound t othe previous
type Name = NonEmpty NameCrumb

data NameCrumb = NameCrumb
  { namespace :: !Symbol
  , name :: !Symbol
  }
  deriving (Eq,Show,Generic)
instance NFData NameCrumb

------------------------------------ Primitive Applicatives/Operatives/Exceptions/Types ------------------------------------

data PrimOp
  = PrimLambda
  | PrimSequence
  | PrimDefineHere
  | PrimList
  | PrimCond
  deriving (Show,Generic)
instance NFData PrimOp

data PrimAp
  = PrimEval
  | PrimEval1 (Loc, Env)
  | PrimDefine
  | PrimDefine3 (Loc, Env)
  | PrimDefine2 (Loc, Env) (Loc, Symbol)
  | PrimDefine1 (Loc, Env) (Loc, Symbol) (Loc, Symbol)
  | PrimLookup
  | PrimLookup2 (Loc, Env)
  | PrimLookup1 (Loc, Env) (Loc, Symbol)
  | PrimForce
  | PrimUnary PrimUnary
  | PrimBin PrimBin
  | PrimBin1 PrimBin (Loc, Value)
  | PrimCaseUnary PrimCaseUnary
  | PrimCaseUnary1 PrimCaseUnary (Loc, Value)
  | PrimCaseBin PrimCaseBin
  | PrimCaseBin2 PrimCaseBin (Loc, Value)
  | PrimCaseBin1 PrimCaseBin (Loc, Value) (Loc, Value)
  | PrimCaseQuat PrimCaseQuat
  | PrimCaseQuat4 PrimCaseQuat (Loc, Value)
  | PrimCaseQuat3 PrimCaseQuat (Loc, Value) (Loc, Value)
  | PrimCaseQuat2 PrimCaseQuat (Loc, Value) (Loc, Value) (Loc, Value)
  | PrimCaseQuat1 PrimCaseQuat (Loc, Value) (Loc, Value) (Loc, Value) (Loc, Value)
  deriving (Show,Generic)
instance NFData PrimAp

data PrimUnary
  = PrimSexprIntro
  | PrimSymIntro
  | PrimSymElim
  | PrimTypeOf
  | PrimNewEnv
  | PrimNewEmptyEnv
  | PrimNameIntro
  deriving (Show,Generic)
instance NFData PrimUnary

data PrimBin
  = PrimEqual
  | PrimAdd
  | PrimSub
  | PrimCons
  | PrimUpdName
  | PrimUpdLoc
  | PrimNameIntro1
  deriving (Show,Generic)
instance NFData PrimBin

data PrimCaseUnary
  = PrimTypeElim
  deriving (Show,Generic)
instance NFData PrimCaseUnary

data PrimCaseBin
  = PrimUncons
  | PrimNameElim
  deriving (Show,Generic)
instance NFData PrimCaseBin

data PrimCaseQuat
  = PrimSexprElim
  deriving (Show,Generic)
instance NFData PrimCaseQuat

data PrimExn
  = ScopeExn Env Name
  | SyntaxExn Sexpr Text
  | UncallableExn Value
  | UnexpectedOperative Callable
  | UnexpectedApplicative Callable
  | TypeError AType Value
  deriving (Show,Generic)
instance NFData PrimExn

-- NOTE These have no type parameters, because they also serve as their own type constructors
data PrimType
  = NilType
  | BoolType
  | IntType
  | StrType
  | SymType
  | ListType
  | SexprType
  | TypeType
  | TyconType
  | EnvType
  | FunType
  | ThunkType
  | PromptType
  | NameType
  | LocType
  deriving (Eq,Show,Generic)
instance NFData PrimType

------------------------------------ Continuations ------------------------------------

data PushPop = Push | Pop

-- almost every continuation also holds the location of the redex's hole,
-- these are marked by the block comment after a location
-- FIXME I should just use named fields
data StackItem :: PushPop -> Type where
  Operate :: Loc -- the whole combination
          -> Loc {- operative -}
          -> Seq Sexpr
          -> StackItem either
  Args :: Loc -- location call was made
       -> Loc {- applicative -}
       -> (NonEmpty Sexpr)
       -> StackItem either
  ArgVals :: Loc -- location call was made
          -> Loc {- applicative -}
          -> (NonEmpty (Loc, Value))
          -> StackItem 'Push
  ArgVal :: Loc -- location call was made
         -> Loc {- applicative -}
         -> (Loc, Value)
         -> StackItem either
  Apply :: Loc -- location call was made
        -> (Loc, Callable)
        -> Loc {- current argument -}
        -> [Sexpr]
        -> StackItem 'Push
  Apply1 :: Loc -- location call was made
         -> (Loc, Callable)
         -> Loc {- argument -}
         -> StackItem 'Pop
  Restore :: ReturnFrom
          -> Env
          -> StackItem either
  Sequence :: Loc {- current statement -}
           -> (NonEmpty Sexpr)
           -> StackItem 'Push
  Then :: Loc {- current statement -}
       -> Sexpr
       -> StackItem 'Pop
  Cond :: Loc {- current predicate -}
       -> Sexpr -- consequent
       -> Seq (Sexpr, Sexpr) -- remaining arcs
       -> StackItem either
  -- supporting primitive operatives
  OpDefineHere :: Env
           -> Loc
           -> Symbol
           -> Loc {- definition body -}
           -> StackItem either
  OpList :: Seq Value
         -> Loc {- current list item -}
         -> Seq Sexpr
         -> StackItem either
  -- fake stack item used for rendering stack traces, but not actually evaluation
  PrimArg :: Int  -- 1-index into the argument values
          -> Loc -- called at
          -> PrimAp
          -> NonEmpty (Loc, Value)
          -> StackItem either
deriving instance Show (StackItem either)

data ReturnFrom
  = FromCall
    { calledAt :: !Loc
    , callee :: !Closure
    }
  | FromEval
    { evaledAt :: !Loc
    , evaleeEnv :: !Env
    , evalee :: !Sexpr
    }
  | FromThunk
    { cell :: !Thunk
    , forcedAt :: !Loc
    , thunkeeEnv :: !Env
    , thunkee :: !Sexpr
    }
  deriving (Show)

data StackTrace = StackTrace (RList TraceItem) (Loc, PrimExn)
  deriving (Show)
data TraceItem
  = CallTrace
    { callerEnv :: !Env
    , calledAt :: !Loc
    , callee :: !Closure
    }
  | EvalTrace
    { evalerEnv :: !Env
    , evaledAt :: !Loc
    , evaleeEnv :: !Env
    , evalee :: !Sexpr
    }
  | ThunkTrace
    { forcerEnv :: !Env
    , forcedAt :: !Loc
    , thunkeeEnv :: !Env
    , thunkee :: !Sexpr
    }
  | PrimArgTrace
      { argNum :: !Int
      , calledAt :: !Loc
      , primFunc :: PrimAp
      }
  deriving (Show)
