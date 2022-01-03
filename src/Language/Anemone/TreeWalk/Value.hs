{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Language.Anemone.TreeWalk.Value
  ( Value(..)
  , Callable(..)
  , Laziness(..)
  , toCallable
  , Closure(..)
  , PrimOp(..)
  , PrimAp(..)
  , PrimUnary(..)
  , PrimBin(..)
  , PrimCaseUnary(..)
  , PrimCaseBin(..)
  , PrimCaseQuat(..)
  , Thunk(..)
  , Control(..)
  , capture
  , PrimExn(..)
  , equal
  , renderName
  ) where

import Data.Foldable (toList)
import Data.List.NonEmpty (NonEmpty(..))
import Data.List.Reverse (RList,snoc)
import Data.Symbol (Symbol)
import Data.Text.Prettyprint.Doc (Doc,Pretty(..),(<+>))
import Data.Zexpr.Location (Loc)
import Language.Anemone.Keywords (valueNamespace,typeNamespace,moduleNamespace)
import Language.Anemone.TreeWalk.Type (AType(..),ATypeInfo(..))
import Language.Anemone.TreeWalk.Unsafe.Types (Callable(..),Closure(..),Laziness(..))
import Language.Anemone.TreeWalk.Unsafe.Types (PrimCaseUnary(..),PrimCaseBin(..),PrimCaseQuat(..))
import Language.Anemone.TreeWalk.Unsafe.Types (PrimExn(..),StackItem(..),PushPop(..))
import Language.Anemone.TreeWalk.Unsafe.Types (PrimOp(..),PrimAp(..))
import Language.Anemone.TreeWalk.Unsafe.Types (PrimUnary(..),PrimBin(..))
import Language.Anemone.TreeWalk.Unsafe.Types (Thunk(..),Env(..),Name,NameCrumb(..))
import Language.Anemone.TreeWalk.Unsafe.Types (Value(..))

import qualified Data.List.NonEmpty as NE
import qualified Data.Sequence as Seq
import qualified Data.Text.Prettyprint.Doc as PP
import qualified Data.Zexpr.Sexpr.Text.Render as Sexpr

toCallable :: Value -> Maybe (Laziness, Callable)
toCallable (PrimOp f) = Just (Strict, OperPrim f)
toCallable (PrimAp f) = Just (Strict, CallPrim f)
toCallable (ClosureVal f@Closure{params=(laziness,_):|_}) =
  Just (laziness, CallClosure f)
toCallable _ = Nothing

capture :: Control -> StackItem 'Push -> Control
capture (PrimCtrl ks exn) k = PrimCtrl (ks `snoc` k) exn

data Control
  = PrimCtrl (RList (StackItem 'Push)) (Loc, PrimExn)
  -- TODO user-defiend control
  deriving(Show)

equal :: Value -> Value -> Bool
equal UnitVal = \case { UnitVal -> True ; _ -> False }
equal (BoolVal a) = \case { BoolVal b -> a == b ; _ -> False }
equal (IntVal a) = \case { IntVal b -> a == b ; _ -> False }
equal (StrVal a) = \case { StrVal b -> a == b ; _ -> False }
equal (SymVal a) = \case { SymVal b -> a == b ; _ -> False }
equal (ListVal a) = \case { ListVal b -> Seq.length a == Seq.length b && and (Seq.zipWith equal a b) ; _ -> False } -- TODO?
equal (SexprVal _) = const False -- TODO?
equal (TypeVal _) = const False -- TODO?
equal (TyconVal a) = \case { TyconVal b -> a == b ; _ -> False }
equal (EnvVal _) = const False -- TODO?
equal (PrimOp _) = const False -- TODO?
equal (PrimAp _) = const False -- TODO?
equal (ClosureVal _) = const False -- TODO?
equal (ThunkVal _) = const False -- TODO?
equal (PrimExn _) = const False -- TODO?
equal (NameVal a) = \case { NameVal b -> a == b ; _ -> False }
equal (LocVal a) = \case { LocVal b -> a == b ; _ -> False }

instance Pretty Value where
  pretty UnitVal = "()"
  pretty (BoolVal True) = "true"
  pretty (BoolVal False) = "false"
  pretty (IntVal n) = PP.viaShow n
  pretty (StrVal s) = PP.pretty $ Sexpr.renderString s
  pretty (SymVal x) = PP.pretty $ Sexpr.renderSymbol x
  pretty (ListVal vs) = PP.encloseSep "[" "]" ", " (pretty <$> toList vs)
  pretty (SexprVal sexpr) = Sexpr.renderPretty sexpr
  pretty (TypeVal ty) = "<type " <> pretty ty <> ">"
  pretty (TyconVal tycon) = "<type constructor " <> PP.viaShow tycon <> ">" -- TODO
  pretty (EnvVal env) = "<" <> pretty env <> ">"
  pretty (PrimOp prim) = "<" <> PP.viaShow prim <> ">"
  pretty (PrimAp prim) = "<" <> PP.viaShow prim <> ">"
  pretty (ClosureVal Closure{name,definedAt}) = "<" <> nameInfo <+> "(" <> pretty definedAt <> ")>"
    where
    nameInfo = case name of
      Just x -> "function" <+> renderName valueNamespace x
      Nothing -> "anonymous function"
  pretty (ThunkVal Thunk{suspendedAt}) = "<thunk (" <> pretty suspendedAt <> ")>" 
  pretty (PrimExn exn) = "<prompt" <+> PP.viaShow exn <> ">" -- TODO
  pretty (NameVal name) = renderName valueNamespace name
  pretty (LocVal l) = "<location " <> pretty l <> ">"

instance Pretty AType where
  pretty AType{name=Just x} = renderName typeNamespace x
  pretty AType{name=Nothing,info} = case info of
    PrimType ty -> PP.viaShow ty
    -- TODO the rest of these

renderName :: Symbol -> Name -> Doc ann
renderName defaultLastNs crumbs =
  let lead = (prettyCrumb <$> NE.init crumbs)
   in PP.encloseSep "" "" ":" $ lead <> [prettyLast (NE.last crumbs)]
  where
  prettyCrumb NameCrumb{namespace,name}
    | namespace == moduleNamespace = prettySymbol name
    | otherwise = "(<namespace" <+> prettySymbol namespace <> ">" <+> prettySymbol name <>")"
  prettyLast NameCrumb{namespace,name}
    | namespace == defaultLastNs = prettySymbol name
    | otherwise = "(<namespace" <+> prettySymbol namespace <> ">" <+> prettySymbol name <>")"

instance Pretty Env where
  pretty Env{name,createdAt} =
    let nameInfo = case name of
          Just x -> "environment" <+> renderName valueNamespace x <> ""
          Nothing -> "anonymous environment"
        locInfo = case createdAt of
          Just loc -> (<+> "(" <> pretty loc <> ")")
          Nothing -> id
    in locInfo nameInfo

prettySymbol :: Symbol -> Doc ann
prettySymbol = pretty . Sexpr.renderSymbol
