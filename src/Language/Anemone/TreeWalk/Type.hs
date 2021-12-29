{-# LANGUAGE NamedFieldPuns #-}

module Language.Anemone.TreeWalk.Type
  ( AType(..)
  , ATypeInfo(..)
  , PrimType(..)
  , typeOf
  , typeElim
  -- * type representations of primitive
  , primNil
  , primBool
  , primInt
  , primStr
  , primSym
  , primList
  , primLoc
  , primSexpr
  , primSexprable
  , primType
  , primTycon
  , primEnv
  , primFun
  , primThunk
  , primPrompt
  ) where

import Data.Sequence (Seq(..))
import Language.Anemone.TreeWalk.Unsafe.Types (AType(..),ATypeInfo(..),Tycon(..),PrimType(..))
import Language.Anemone.TreeWalk.Unsafe.Types (Value(..))

import qualified Data.Sequence as Seq

typeOf :: Value -> AType
typeOf NilVal = primNil
typeOf (BoolVal _) = primBool
typeOf (IntVal _) = primInt
typeOf (StrVal _) = primStr
typeOf (SymVal _) = primSym
typeOf (ListVal _) = primList
typeOf (LocVal _) = primLoc
typeOf (SexprVal _) = primSexpr
typeOf (TypeVal _) = primType
typeOf (TyconVal _) = primTycon
typeOf (EnvVal _) = primEnv
typeOf (PrimOp _) = primFun
typeOf (PrimAp _) = primFun
typeOf (ClosureVal _) = primFun
typeOf (ThunkVal _) = primThunk
typeOf (PrimExn _) = primPrompt

typeElim :: AType -> (Tycon, Seq Value)
typeElim AType{info} = go info
  where
  go (PrimType a) = (PrimTycon a, Empty)
  go (UnionTy tys) = (UnionTycon, Seq.singleton (ListVal $ TypeVal <$> tys))

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

primList :: AType
primList = AType { info = PrimType ListType }

primLoc :: AType
primLoc = AType { info = PrimType LocType }

primSexpr :: AType
primSexpr = AType { info = PrimType SexprType }

primSexprable :: AType
primSexprable = AType{ info = UnionTy (Seq.fromList [primInt, primStr, primSym, primList]) } -- FIXME the list type here should be parameterized by Sexprable

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
