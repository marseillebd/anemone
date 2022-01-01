{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}

module Language.Anemone.TreeWalk.Type
  ( AType(..)
  , ATypeInfo(..)
  , PrimType(..)
  , typeOf
  , typeElim
  -- * type representations of primitives
  , primNil
  , primBool
  , primInt
  , primStr
  , primSym
  , primList
  , primSexpr
  , primType
  , primTycon
  , primEnv
  , primFun
  , primThunk
  , primPrompt
  , primName
  , primLoc
  , primSexprable
  , primNameIntroable
  , primAny
  ) where

import Data.List.NonEmpty (NonEmpty(..))
import Data.Sequence (Seq(..))
import Data.Symbol (intern)
import Data.Zexpr.Location (Loc(..))
import Data.Zexpr.Sexpr (Sexpr(..),Atom(..))
import Language.Anemone.Keywords (valueNamespace)
import Language.Anemone.TreeWalk.Environment (Env,newEmptyEnv)
import Language.Anemone.TreeWalk.Unsafe.Types (AType(..),ATypeInfo(..),Tycon(..),PrimType(..))
import Language.Anemone.TreeWalk.Unsafe.Types (NameCrumb(..))
import Language.Anemone.TreeWalk.Unsafe.Types (Value(..),Callable(..),Closure(..),Laziness(..))
import System.IO.Unsafe (unsafePerformIO)

import qualified Data.Sequence as Seq
import qualified Data.List.Reverse as RList

typeOf :: Value -> AType
typeOf NilVal = primNil
typeOf (BoolVal _) = primBool
typeOf (IntVal _) = primInt
typeOf (StrVal _) = primStr
typeOf (SymVal _) = primSym
typeOf (ListVal _) = primList primAny
typeOf (SexprVal _) = primSexpr
typeOf (TypeVal _) = primType
typeOf (TyconVal _) = primTycon
typeOf (EnvVal _) = primEnv
typeOf (PrimOp _) = primFun
typeOf (PrimAp _) = primFun
typeOf (ClosureVal _) = primFun
typeOf (ThunkVal _) = primThunk
typeOf (PrimExn _) = primPrompt
typeOf (NameVal _) = primName
typeOf (LocVal _) = primLoc

typeElim :: AType -> (Tycon, Seq Value)
typeElim AType{info} = go info
  where
  go (PrimType a) = (PrimTycon a, Empty)
  go (ForallType f) = (ForallTycon, Seq.singleton (fromCallable f))
  go (ExistsType f) = (ExistsTycon, Seq.singleton (fromCallable f))
  go (ListType t) = (UnionTycon, Seq.singleton (TypeVal t))
  go (UnionType tys) = (UnionTycon, Seq.singleton (ListVal $ TypeVal <$> tys))
  fromCallable (OperPrim f) = PrimOp f
  fromCallable (CallPrim f) = PrimAp f
  fromCallable (CallClosure f) = ClosureVal f

primNil :: AType
primNil = AType { info = PrimType NilType }

primBool :: AType
primBool = AType { info = PrimType BoolType }

primInt :: AType
primInt = AType { info = PrimType IntType }

primStr :: AType
primStr = AType { info = PrimType StrType }

primSym :: AType
primSym = AType { info = PrimType SymType }

primList :: AType -> AType
primList t = AType { info = ListType t }

primSexpr :: AType
primSexpr = AType { info = PrimType SexprType }

primType :: AType
primType = AType { info = PrimType TypeType }

primTycon :: AType
primTycon = AType { info = PrimType TyconType }

primEnv :: AType
primEnv = AType { info = PrimType EnvType }

primFun :: AType
primFun = AType { info = PrimType FunType }

primThunk :: AType
primThunk = AType { info = PrimType ThunkType }

primPrompt :: AType
primPrompt = AType { info = PrimType PromptType }

primName :: AType
primName = AType { info = PrimType NameType }

primLoc :: AType
primLoc = AType { info = PrimType LocType }

primSexprable :: AType
primSexprable = AType{ info = UnionType (Seq.fromList [primInt, primStr, primSym, primList primSexpr]) }

primNameIntroable :: AType
primNameIntroable = AType { info = UnionType (Seq.fromList [primSym, primList primName]) }

primAny :: AType
primAny = AType { info = ExistsType identity }
  where
  identity :: Callable
  identity = CallClosure $ Closure
    { name = Just $ NameCrumb valueNamespace (intern "identity") :| []
    , definedAt = LocUnknown
    , scope = unsafeEmptyEnv
    , args = RList.nil
    , params = (Strict, x):|[]
    , body = SAtom LocUnknown (Sym x)
    }
  x = intern "x"


unsafeEmptyEnv :: Env
{-# NOINLINE unsafeEmptyEnv #-}
unsafeEmptyEnv = unsafePerformIO $ newEmptyEnv
