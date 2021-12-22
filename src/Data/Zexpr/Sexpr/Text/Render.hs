{-# LANGUAGE GADTs #-}

module Data.Zexpr.Sexpr.Text.Render
  ( renderPlain
  , renderAtom
  ) where

import Prelude hiding (exp)

import Data.Zexpr.Sexpr (Sexpr(..), Atom(..))

import Data.List (intercalate)
import Data.Symbol (unintern)

renderPlain :: Sexpr -> String
renderPlain (SAtom _ a) = renderAtom a
renderPlain (SCombo _ es) = concat ["(", intercalate " " (renderPlain <$> es), ")"]

renderAtom :: Atom -> String
renderAtom (Sym x) = unintern x -- FIXME but check that the symbol doesn't need escaping
renderAtom (Int n) = show n
renderAtom (Str s) = show s -- FIXME but escape using the zexpr rules, not Haskell's
