{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Language.Bslisp.TreeWalk.Value
  ( Value(..)
  , Callable(..)
  , toCallable
  , Closure(..)
  , PrimOp(..)
  , PrimAp(..)
  , PrimUnary(..)
  , PrimBin(..)
  , PrimCaseBin(..)
  , PrimCaseQuat(..)
  , Control(..)
  , capture
  , PrimExn(..)
  ) where

import Data.List.Reverse (RList,snoc)
import Data.Text.Prettyprint.Doc ((<+>))
import Data.Text.Prettyprint.Doc (Pretty(..))
import Data.Zexpr.Location (Loc(..))
import Language.Bslisp.TreeWalk.Unsafe.Types (Callable(..),Closure(..))
import Language.Bslisp.TreeWalk.Unsafe.Types (PrimCaseBin(..),PrimCaseQuat(..))
import Language.Bslisp.TreeWalk.Unsafe.Types (PrimExn(..),StackItem(..),PushPop(..))
import Language.Bslisp.TreeWalk.Unsafe.Types (PrimOp(..),PrimAp(..))
import Language.Bslisp.TreeWalk.Unsafe.Types (PrimUnary(..),PrimBin(..))
import Language.Bslisp.TreeWalk.Unsafe.Types (Value(..))

import qualified Data.Text.Prettyprint.Doc as PP
import qualified Data.Zexpr.Sexpr.Text.Render as Sexpr

toCallable :: Value -> Maybe Callable
toCallable (PrimOp f) = Just (OperPrim f)
toCallable (PrimAp f) = Just (CallPrim f)
toCallable (ClosureVal f) = Just (CallClosure f)
toCallable _ = Nothing

capture :: Control -> StackItem 'Push -> Control
capture (PrimCtrl ks exn) k = PrimCtrl (ks `snoc` k) exn

data Control
  = PrimCtrl (RList (StackItem 'Push)) PrimExn
  -- TODO user-defiend control
  deriving(Show)

instance Pretty Value where
  pretty NilVal = "()"
  pretty (IntVal n) = PP.viaShow n
  pretty (StrVal s) = PP.pretty $ Sexpr.renderString s
  pretty (SymVal x) = PP.pretty $ Sexpr.renderSymbol x
  pretty (ListVal vs) = PP.encloseSep "[" "]" ", " (pretty <$> vs)
  pretty (LocVal l) = "loc(" <> pretty l <> ")"
  pretty (SexprVal sexpr) = Sexpr.renderPretty sexpr
  pretty (EnvVal env) = "<Env>" -- TODO
  pretty (PrimOp prim) = "<" <> PP.viaShow prim <> ">"
  pretty (PrimAp prim) = "<" <> PP.viaShow prim <> ">"
  pretty (ClosureVal Closure{name,definedAt}) = "<" <> nameInfo <+> "(" <> pretty definedAt <> ")>"
    where
    nameInfo = case name of
      Just x -> "function " <+> pretty (Sexpr.renderSymbol x)
      Nothing -> "anonymous function"

instance Pretty Loc where
  pretty loc =   PP.viaShow (filename loc)
             <+>        pretty (fromLine loc) <> ":" <> pretty (fromCol loc)
             <>  "-" <> pretty (toLine loc) <> ":" <> pretty (toCol loc)
