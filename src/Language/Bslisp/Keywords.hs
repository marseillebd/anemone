module Language.Bslisp.Keywords
  ( operate
  , quote
  ) where

import Data.Symbol (Symbol, intern)

operate :: Symbol
operate = intern "__operate__"


quote :: Symbol
quote = intern "__quote__"
