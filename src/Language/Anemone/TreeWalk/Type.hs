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
  , primSexpry
  , primNamey
  , primAny
  ) where

import Data.List.NonEmpty (NonEmpty(..))
import Data.Sequence (Seq(..))
import Data.Symbol (intern)
import Data.Zexpr.Location (Loc(..))
import Data.Zexpr.Sexpr (Sexpr(..),Atom(..))
import Language.Anemone.Keywords (typeNamespace)
import Language.Anemone.TreeWalk.Environment (Env,newEmptyEnv)
import Language.Anemone.TreeWalk.Unsafe.Types (AType(..),ATypeInfo(..),Tycon(..),PrimType(..))
import Language.Anemone.TreeWalk.Unsafe.Types (NameCrumb(..))
import Language.Anemone.TreeWalk.Unsafe.Types (Value(..),Closure(..),Laziness(..))
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
  go (ForallType f) = (ForallTycon, Seq.singleton (ClosureVal f))
  go (ExistsType f) = (ExistsTycon, Seq.singleton (ClosureVal f))
  go (ListType t) = (UnionTycon, Seq.singleton (TypeVal t))
  go (UnionType tys) = (UnionTycon, Seq.singleton (ListVal $ TypeVal <$> tys))

primNil :: AType
primNil = _atype "Nil" $ PrimType NilType

primBool :: AType
primBool = _atype "Bool" $ PrimType BoolType

primInt :: AType
primInt = _atype "Integer" $ PrimType IntType

primStr :: AType
primStr = _atype "String" $ PrimType StrType

primSym :: AType
primSym = _atype "Symbol" $ PrimType SymType

primList :: AType -> AType
primList t = _atype "List" $ ListType t

primSexpr :: AType
primSexpr = _atype "Sexpr" $ PrimType SexprType

primType :: AType
primType = _atype "Type" $ PrimType TypeType

primTycon :: AType
primTycon = _atype "TypeCtor" $ PrimType TyconType

primEnv :: AType
primEnv = _atype "Env" $ PrimType EnvType

primFun :: AType
primFun = _atype "Function" $ PrimType FunType

primThunk :: AType
primThunk = _atype "Thunk" $ PrimType ThunkType

primPrompt :: AType
primPrompt = _atype "Prompt" $ PrimType PromptType

primName :: AType
primName = _atype "Name" $ PrimType NameType

primLoc :: AType
primLoc = _atype "Loc" $ PrimType LocType

primSexpry :: AType
primSexpry = _atype "Sexpry" $ UnionType (Seq.fromList [primInt, primStr, primSym, primList primSexpr])

primNamey :: AType
primNamey = _atype "Namey" $ UnionType (Seq.fromList [primSym, primList primName])

primAny :: AType
primAny = _atype "Any" $ ExistsType identityType

identityType :: Closure
identityType = Closure
  { name = Just $ NameCrumb typeNamespace (intern "Identity") :| []
  , definedAt = LocUnknown
  , scope = unsafeEmptyEnv
  , args = RList.nil
  , params = (Strict, t):|[]
  , body = SAtom LocUnknown (Sym t)
  }
  where
  t = intern "t"

unsafeEmptyEnv :: Env
{-# NOINLINE unsafeEmptyEnv #-}
unsafeEmptyEnv = unsafePerformIO $ newEmptyEnv


_atype :: String -> ATypeInfo -> AType
_atype str info = AType
  { info
  , name = Just $ NameCrumb typeNamespace (intern str) :| []
  , definedAt = LocUnknown
  }
