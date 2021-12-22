module Data.Zexpr.Sexpr
  ( Atom(..)
  , Sexpr(..)
  , loc
  ) where

import Data.Zexpr.Location (Loc(..))
import Data.Symbol (Symbol)
import Data.Text (Text)

------------------------------------ Atoms ------------------------------------

data Atom
  = Sym Symbol
  | Int Integer
  -- NOTE floating point is represented in the source as just (syntactic sugar for) a function call of integers
  | Str Text
  deriving (Show,Eq)

------------------------------------ S-Expressions ------------------------------------

data Sexpr
  = SAtom Loc Atom
  | SCombo Loc [Sexpr]
  deriving (Show,Eq)

loc :: Sexpr -> Loc
loc (SAtom l _) = l
loc (SCombo l _) = l
