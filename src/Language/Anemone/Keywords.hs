module Language.Anemone.Keywords
  ( operate
  , quote
  -- * built-in namespaces
  , valueNamespace
  , moduleNamespace
  ) where

import Data.Symbol (Symbol, intern)

operate :: Symbol
operate = intern "__operate__"

quote :: Symbol
quote = intern "__quote__"

valueNamespace :: Symbol
valueNamespace = intern "value"

moduleNamespace :: Symbol
moduleNamespace = intern "module"
