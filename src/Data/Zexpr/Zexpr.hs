{-# LANGUAGE GADTs #-}

module Data.Zexpr.Zexpr
  ( Zexpr(..)
  , Atom(..)
  , Combine(..)
  , loc
  ) where

import Data.Symbol (Symbol)
import Data.Text (Text)
import Data.Zexpr.Location (Loc(..))
import Data.Zexpr.Sexpr (Atom(..))

data Zexpr where
  ZAtom :: Loc -> Atom -> Zexpr
  ZCombo :: Loc -> Combine subexprs -> subexprs -> Zexpr

data Combine subexpr where
  Dollar :: Combine ()
  QualName :: Combine [(Loc, Symbol)]
  Round :: Combine [Zexpr]
  Square :: Combine (Loc, [Zexpr])
  ConsDot :: Combine ([Zexpr], Loc, Zexpr)
  LensField :: Combine (Zexpr, Loc, Symbol)
  LensIndex :: Combine (Zexpr, Zexpr)
  FloatLit :: Combine (Integer, Int, Integer)
  MakeInt :: Combine (Zexpr, Loc, Integer)
  MakeFloat :: Combine (Zexpr, Loc, (Integer, Int, Integer))
  MakeStr :: Combine (Zexpr, Loc, Text)
  MakeList :: Combine (Zexpr, Loc, [Zexpr])
  Tick :: Combine (Loc, Zexpr)
  Backtick :: Combine (Loc, Zexpr)
  Comma :: Combine (Loc, Zexpr)
  CommaAt :: Combine (Loc, Zexpr)

loc :: Zexpr -> Loc
loc (ZAtom l _) = l
loc (ZCombo l _ _) = l
