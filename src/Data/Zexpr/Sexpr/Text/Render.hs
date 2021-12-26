{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}

module Data.Zexpr.Sexpr.Text.Render
  ( renderPlain
  , renderPretty
  , renderAtom
  , renderString
  , renderSymbol
  ) where

import Prelude hiding (exp)

import Data.Bits ((.&.), unsafeShiftR)
import Data.Char (ord, isPrint)
import Data.List (intercalate)
import Data.Symbol (Symbol,unintern)
import Data.Text (Text)
import Data.Text.Prettyprint.Doc (Doc,pretty,group,(<+>),sep,vsep,flatAlt,align,nest)
import Data.Zexpr.Sexpr (Sexpr(..), Atom(..))
import Data.Zexpr.Text.Parser (isSymChar)
import Numeric (showHex)

import qualified Data.Text as T
import qualified Data.Text.Prettyprint.Doc as PP

renderPlain :: Sexpr -> String
renderPlain (SAtom _ a) = renderAtom a
renderPlain (SCombo _ es) = concat ["(", intercalate " " (renderPlain <$> es), ")"]

renderPretty :: Sexpr -> Doc ann
renderPretty = group . go
  where
  go (SAtom _ a) = pretty $ renderAtom a
  go (SCombo _ es) = funtime es
  standard [] = "()"
  standard [e] = "(" <> group (renderPretty e) <> ")"
  standard (e:es) =
    let inline = PP.encloseSep "" "" PP.space (renderPretty <$> es)
        multiline = vsep (group . renderPretty <$> es)
     in "(" <> group (renderPretty e) <+> align (flatAlt multiline inline) <> ")"
  miser [] = "()"
  miser es =
    let inline = PP.encloseSep "(" ")" PP.space (renderPretty <$> es)
        multiline = vsep (group . renderPretty <$> es)
     in "(" <> align (flatAlt multiline inline) <> ")"
  funtime :: [Sexpr] -> Doc ann
  funtime [] = "()"
  funtime [e] = "(" <> group (renderPretty e) <> ")"
  funtime (e:es) =
    let (beforeCombos, combos) = takeLastCombos es
        inline = PP.encloseSep "" "" PP.space (renderPretty <$> (e:es))
        lineone = group (renderPretty e) <+> align (sep $ group . renderPretty <$> beforeCombos)
        combolines = vsep $ group . renderPretty <$> combos
        multiline = case combos of
          [] -> lineone
          _ -> lineone <> nest 2 (PP.hardline <> combolines)
     in "(" <> flatAlt multiline inline <> ")"
  takeLastCombos = comboLoop [] [] . reverse
    where
    comboLoop [] combos [] = ([], combos)
    comboLoop [] combos (e:rest) = case e of
      SAtom _ _ -> (reverse (e:rest), combos)
      SCombo _ _ -> comboLoop [] (e:combos) rest

renderAtom :: Atom -> String
renderAtom (Int n) = show n
renderAtom (Str s) = renderString s
renderAtom (Sym x) = renderSymbol x

renderString :: Text -> String
renderString (T.unpack -> str) = concat ["\"", concatMap go str, "\""]
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

renderSymbol :: Symbol -> String
renderSymbol x =
  let name = unintern x
   in if all isSymChar name then name else '\\':renderString (T.pack name)
