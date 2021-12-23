{-# LANGUAGE GADTs #-}

module Data.Zexpr.Sexpr.Text.Render
  ( renderPlain
  , renderAtom
  ) where

import Prelude hiding (exp)

import Data.Bits ((.&.), unsafeShiftR)
import Data.Char (ord, isPrint)
import Data.List (intercalate)
import Data.Symbol (unintern)
import Data.Zexpr.Sexpr (Sexpr(..), Atom(..))
import Data.Zexpr.Text.Parser (isSymChar)
import Numeric (showHex)

import qualified Data.Text as T

renderPlain :: Sexpr -> String
renderPlain (SAtom _ a) = renderAtom a
renderPlain (SCombo _ es) = concat ["(", intercalate " " (renderPlain <$> es), ")"]

renderAtom :: Atom -> String
renderAtom (Sym x) =
  let name = unintern x
   in if all isSymChar name then name else '\\':renderString name
renderAtom (Int n) = show n
renderAtom (Str s) = renderString (T.unpack s)

renderString :: String -> String
renderString str = concat ["\"", concatMap go str, "\""]
  where
  go c | isPrint c = [c]
  go '\ESC' = "\\e"
  go '\n' = "\\n"
  go '\r' = "\\r"
  go '\t' = "\\t"
  go '\'' = "\\\'"
  go '\"' = "\\\""
  go '\\' = "\\\\"
  go c | c < '\x80' =
    let hex = ord c
        (hi, lo) = (hex `unsafeShiftR` 4, hex .&. 0xF)
     in concat ["\\x", showHex hi "", showHex lo ""]
  go c = concat ["\\U+", showHex (ord c) "", ";"]
