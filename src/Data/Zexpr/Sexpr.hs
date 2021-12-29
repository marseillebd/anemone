{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StandaloneDeriving #-}

module Data.Zexpr.Sexpr
  ( Atom(..)
  , Sexpr(..)
  , loc
  ) where

import Control.DeepSeq (NFData)
import Data.Sequence (Seq)
import Data.Symbol.Unsafe (Symbol(..))
import Data.Text (Text)
import Data.Zexpr.Location (Loc(..))
import GHC.Generics (Generic)

deriving instance Generic Symbol
instance NFData Symbol

------------------------------------ Atoms ------------------------------------

data Atom
  = Sym Symbol
  | Int Integer
  -- NOTE floating point is represented in the source as just (syntactic sugar for) a function call of integers
  | Str Text
  deriving (Show,Eq,Generic)
instance NFData Atom

------------------------------------ S-Expressions ------------------------------------

data Sexpr
  = SAtom Loc Atom
  | SCombo Loc (Seq Sexpr)
  deriving (Show,Eq,Generic)
instance NFData Sexpr

loc :: Sexpr -> Loc
loc (SAtom l _) = l
loc (SCombo l _) = l
