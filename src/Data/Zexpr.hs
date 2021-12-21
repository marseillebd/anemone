{-# LANGUAGE GADTs #-}

module Data.Zexpr
  ( Loc(..)
  , joinLoc
  , Atom(..)
  , Sexpr(..)
  , Zexpr(..)
  , Combine(..)
  , zexprLoc
  , crapDisplay -- DEBUG
  ) where

import Data.ByteString (ByteString)
import Data.List (intercalate)
import Data.Symbol (Symbol,unintern)

import qualified Data.Text as T
import qualified Data.Text.Encoding as T

------------------------------------ Location Reporting ------------------------------------

data Loc
  = LocUnknown
  | Loc
    { filename :: FilePath
    , fromLine :: Int
    , fromCol :: Int
    , toLine :: Int
    , toCol :: Int
    }
  deriving(Show)

joinLoc :: Loc -> Loc -> Loc
joinLoc l1 l2 = Loc
  { filename = filename l1
  , fromLine = fromLine l1
  , fromCol = fromCol l1
  , toLine = toLine l2
  , toCol = toCol l2
  }

------------------------------------ Atoms ------------------------------------

data Atom
  = Sym Symbol
  | Int Integer
  -- NOTE floating point is represented in the source as just (syntactic sugar for) a function call of integers
  | Str ByteString
  deriving (Show)

------------------------------------ S-Expressions ------------------------------------

data Sexpr
  = SAtom Loc Atom
  | SCombo Loc [Sexpr]

------------------------------------ Z-Expressions ------------------------------------

data Zexpr where
  ZAtom :: Loc -> Atom -> Zexpr
  ZCombo :: Loc -> Combine subexprs -> subexprs -> Zexpr

zexprLoc :: Zexpr -> Loc
zexprLoc (ZAtom loc _) = loc
zexprLoc (ZCombo loc _ _) = loc

data Combine subexpr where
  Round :: Combine [Zexpr]
  Square :: Combine [Zexpr]
  ConsDot :: Combine ([Zexpr], Zexpr)
  LensField :: Combine (Zexpr, Symbol)
  LensIndex :: Combine (Zexpr, [Zexpr])
  Dollar :: Combine ()
  Tick :: Combine (Loc, Zexpr)
  Backtick :: Combine (Loc, Zexpr)
  Comma :: Combine (Loc, Zexpr)
  CommaAt :: Combine (Loc, Zexpr)
  QualName :: Combine [(Loc, Symbol)]

crapDisplay :: Zexpr -> String
crapDisplay (ZAtom _ (Sym x)) = unintern x
crapDisplay (ZAtom _ (Int n)) = show n
crapDisplay (ZAtom _ (Str s)) = show $ T.unpack (T.decodeUtf8 s)
crapDisplay (ZCombo _ Round es) = concat ["(", intercalate " " (crapDisplay <$> es), ")"]
crapDisplay (ZCombo _ Square es) = concat ["[", intercalate " " (crapDisplay <$> es), "]"]
-- crapDisplay (ZCombo _ Curly es) = concat ["{", intercalate " " (crapDisplay <$> es), "}"]
crapDisplay (ZCombo _ ConsDot (es, e')) = concat ["(", intercalate " " (crapDisplay <$> es), " . ", crapDisplay e', ")"]
crapDisplay (ZCombo _ LensField (e, f)) = concat [crapDisplay e, ".", unintern f]
crapDisplay (ZCombo _ LensIndex (e, ix)) = concat [crapDisplay e, "[", intercalate " " (crapDisplay <$> ix), "]"]
crapDisplay (ZCombo _ Dollar ()) = "$"
crapDisplay (ZCombo _ Tick (_, e)) = concat ["'", crapDisplay e]
crapDisplay (ZCombo _ Backtick (_, e)) = concat ["`", crapDisplay e]
crapDisplay (ZCombo _ Comma (_, e)) = concat [",", crapDisplay e]
crapDisplay (ZCombo _ CommaAt (_, e)) = concat [",@", crapDisplay e]
crapDisplay (ZCombo _ QualName xs) = intercalate ":" (unintern . snd <$> xs)
