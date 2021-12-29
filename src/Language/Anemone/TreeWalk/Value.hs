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
  ) where

import Data.Foldable (toList)
import Data.List.NonEmpty (NonEmpty(..))
import Data.List.Reverse (RList,snoc)
import Data.Text.Prettyprint.Doc (Pretty(..),(<+>))
import Data.Zexpr.Location (Loc)
import Language.Anemone.TreeWalk.Type (AType(..))
import Language.Anemone.TreeWalk.Unsafe.Types (Callable(..),Closure(..),Laziness(..))
import Language.Anemone.TreeWalk.Unsafe.Types (PrimCaseUnary(..),PrimCaseBin(..),PrimCaseQuat(..))
import Language.Anemone.TreeWalk.Unsafe.Types (PrimExn(..),StackItem(..),PushPop(..))
import Language.Anemone.TreeWalk.Unsafe.Types (PrimOp(..),PrimAp(..))
import Language.Anemone.TreeWalk.Unsafe.Types (PrimUnary(..),PrimBin(..))
import Language.Anemone.TreeWalk.Unsafe.Types (Thunk(..),Env(..))
import Language.Anemone.TreeWalk.Unsafe.Types (Value(..))

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
equal NilVal = \case { NilVal -> True ; _ -> False }
equal (BoolVal a) = \case { BoolVal b -> a == b ; _ -> False }
equal (IntVal a) = \case { IntVal b -> a == b ; _ -> False }
equal (StrVal a) = \case { StrVal b -> a == b ; _ -> False }
equal (SymVal a) = \case { SymVal b -> a == b ; _ -> False }
equal (ListVal a) = \case { ListVal b -> Seq.length a == Seq.length b && and (Seq.zipWith equal a b) ; _ -> False } -- TODO?
equal (LocVal a) = \case { LocVal b -> a == b ; _ -> False }
equal (SexprVal _) = const False -- TODO?
equal (TypeVal _) = const False -- TODO?
equal (TyconVal a) = \case { TyconVal b -> a == b ; _ -> False }
equal (EnvVal _) = const False -- TODO?
equal (PrimOp _) = const False -- TODO?
equal (PrimAp _) = const False -- TODO?
equal (ClosureVal _) = const False -- TODO?
equal (ThunkVal _) = const False -- TODO?
equal (PrimExn _) = const False -- TODO?

instance Pretty Value where
  pretty NilVal = "()"
  pretty (BoolVal True) = "true"
  pretty (BoolVal False) = "false"
  pretty (IntVal n) = PP.viaShow n
  pretty (StrVal s) = PP.pretty $ Sexpr.renderString s
  pretty (SymVal x) = PP.pretty $ Sexpr.renderSymbol x
  pretty (ListVal vs) = PP.encloseSep "[" "]" ", " (pretty <$> toList vs)
  pretty (LocVal l) = "<location " <> pretty l <> ">"
  pretty (SexprVal sexpr) = Sexpr.renderPretty sexpr
  pretty (TypeVal AType{info}) = "<type " <> PP.viaShow info <> ">" -- TODO
  pretty (TyconVal tycon) = "<type constructor " <> PP.viaShow tycon <> ">" -- TODO
  pretty (EnvVal env) = "<" <> pretty env <> ">"
  pretty (PrimOp prim) = "<" <> PP.viaShow prim <> ">"
  pretty (PrimAp prim) = "<" <> PP.viaShow prim <> ">"
  pretty (ClosureVal Closure{name,definedAt}) = "<" <> nameInfo <+> "(" <> pretty definedAt <> ")>"
    where
    nameInfo = case name of
      Just x -> "function" <+> pretty (Sexpr.renderSymbol x)
      Nothing -> "anonymous function"
  pretty (ThunkVal Thunk{suspendedAt}) = "<thunk (" <> pretty suspendedAt <> ")>" 
  pretty (PrimExn exn) = "<prompt" <+> PP.viaShow exn <> ">" -- TODO

instance Pretty Env where
  pretty Env{name,createdAt} =
    let nameInfo = case name of
          Just x -> "environment" <+> pretty (Sexpr.renderSymbol x) <> ""
          Nothing -> "anonymous environment"
        locInfo = case createdAt of
          Just loc -> (<+> "(" <> pretty loc <> ")")
          Nothing -> id
    in locInfo nameInfo
