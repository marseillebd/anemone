module Language.Bslisp.TreeWalk.Type
  ( AType(..)
  , ATypeInfo(..)
  , PrimType(..)
  , typeOf
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
  , primEnv
  , primFun
  , primThunk
  ) where

import Language.Bslisp.TreeWalk.Unsafe.Types (AType(..),ATypeInfo(..),PrimType(..))
import Language.Bslisp.TreeWalk.Unsafe.Types (Value(..))

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
typeOf (EnvVal _) = primEnv
typeOf (PrimOp _) = primFun
typeOf (PrimAp _) = primFun
typeOf (ClosureVal _) = primFun
typeOf (ThunkVal _) = primThunk

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
primSexprable = AType{ info = UnionTy [primInt, primStr, primSym, primList] } -- FIXME the list type here should be parameterized by Sexprable

primType :: AType
primType = AType { info = PrimType TypeType }

primEnv :: AType
primEnv = AType { info = PrimType EnvType }

primFun :: AType
primFun = AType { info = PrimType FunType }

primThunk :: AType
primThunk = AType { info = PrimType ThunkType }
