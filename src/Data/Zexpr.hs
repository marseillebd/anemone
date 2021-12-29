{-# LANGUAGE GADTs #-}

module Data.Zexpr
  ( Loc(..)
  , Atom(..)
  , Sexpr(..)
  , Zexpr(..)
  , Combine(..)
  , loc
  -- * Conversion
  , toSexpr
  , Conf(..)
  , defaultConf
  -- * Symbols
  , Symbol, intern, unintern
  ) where

import Prelude hiding (exp)

import Data.Sequence (Seq(..))
import Data.Symbol (Symbol,intern,unintern)
import Data.Zexpr.Location (Loc(..))
import Data.Zexpr.Sexpr (Atom(..),Sexpr(..))
import Data.Zexpr.Zexpr (Zexpr(..),Combine(..),loc)

import qualified Data.Sequence as Seq

data Conf = Conf
  { operativeName :: String
  , qualifyName :: String
  , qualifyIsOperative :: Bool
  , squareName :: String
  , squareIsOperative :: Bool
  , improperListName :: String
  , improperListIsOperative :: Bool
  , lensFieldName :: String
  , lensFieldIsOperative :: Bool
  , lensIndexName :: String
  , lensIndexIsOperative :: Bool
  , floatLiteralName :: String
  , floatLiteralIsOperative :: Bool
  , overloadedIntegerOperativeName :: Maybe String
  , overloadedFloatOperativeName :: Maybe String
  , overloadedStringOperativeName :: Maybe String
  , overloadedListOperativeName :: Maybe String
  , tickName :: String
  , tickIsOperative :: Bool
  , backtickName :: String
  , backtickIsOperative :: Bool
  , commaName :: String
  , commaIsOperative :: Bool
  , commaAtName :: String
  , commaAtIsOperative :: Bool
  }

defaultConf :: Conf
defaultConf = Conf
  { operativeName = "__operate__"
  , qualifyName = "__qualName__"
  , qualifyIsOperative = True
  , squareName = "__list__"
  , squareIsOperative = True
  , improperListName = "__improper-list__"
  , improperListIsOperative = True
  , lensFieldName = "__lensField__"
  , lensFieldIsOperative = True
  , lensIndexName = "__lensIndex__"
  , lensIndexIsOperative = False
  , floatLiteralName = "__mkDefaultFloat__"
  , floatLiteralIsOperative = False
  , overloadedIntegerOperativeName = Just "__mkInt__"
  , overloadedFloatOperativeName = Just "__mkFloat__"
  , overloadedStringOperativeName = Just "__mkString__"
  , overloadedListOperativeName = Just "__mkList__"
  , tickName = "__quote__"
  , tickIsOperative = True
  , backtickName = "__quasiquote__"
  , backtickIsOperative = True
  , commaName = "__unquote__"
  , commaIsOperative = True
  , commaAtName = "__unquote-splicing__"
  , commaAtIsOperative = True
  }


toSexpr :: Conf -> Zexpr -> Sexpr
toSexpr conf = go
  where
  go (ZAtom l a) = SAtom l a
  go (ZCombo l Dollar ()) = SAtom l (Sym operativeSymbol)
  go (ZCombo l QualName ss) =
    let op = SCombo l (SAtom l (Sym operativeSymbol) :<| ap :<| Empty)
        ap = SAtom l (Sym qualifySymbol)
        args = (\(lS, s) -> SAtom lS (Sym s)) <$> ss
     in if qualifyIsOperative conf
        then SCombo l (op :<| args)
        else SCombo l (ap :<| args)
  go (ZCombo l Round es) = SCombo l (go <$> es)
  go (ZCombo l Square (lSq, es)) =
    let op = SCombo l (SAtom lSq (Sym operativeSymbol) :<| ap :<| Empty)
        ap = SAtom lSq $ Sym squareSymbol
        args = go <$> es
     in if squareIsOperative conf
        then SCombo l (op :<| args)
        else SCombo l (ap :<| args)
  go (ZCombo l ConsDot (es, lDot, e')) =
    let op = SCombo l (SAtom lDot (Sym operativeSymbol) :<| ap :<| Empty)
        ap = SAtom lDot $ Sym improperListSymbol
        args = (go <$> es) :|> go e'
     in if improperListIsOperative conf
        then SCombo l (op :<| args)
        else SCombo l (ap :<| args)
  go (ZCombo l LensField (e, lF, f)) =
    let op = SCombo l (SAtom l (Sym operativeSymbol) :<| ap :<| Empty)
        fd = SAtom lF (Sym f)
        ap = SAtom lF (Sym lensFieldSymbol)
        args = go e :<| fd :<| Empty
     in if lensFieldIsOperative conf
        then SCombo l (op :<| args)
        else SCombo l (ap :<| args)
  go (ZCombo l LensIndex (e, ix)) =
    let op = SCombo l (SAtom (loc ix) (Sym operativeSymbol) :<| ap :<| Empty)
        ap = SAtom (loc ix) (Sym lensIndexSymbol)
        args = go e :<| go ix :<| Empty
     in if lensIndexIsOperative conf
        then SCombo l (op :<| args)
        else SCombo l (ap :<| args)
  go (ZCombo l FloatLit (sig, base, exp)) =
    let op = SCombo l (SAtom l (Sym operativeSymbol) :<| ap :<| Empty)
        ap = SAtom l (Sym floatLiteralSymbol)
        args = SAtom l (Int sig) :<| SAtom l (Int $ fromIntegral base) :<| SAtom l (Int exp) :<| Empty
     in if floatLiteralIsOperative conf
        then SCombo l (op :<| args)
        else SCombo l (ap :<| args)
  go (ZCombo l MakeInt (e, lN, n)) =
    let args = go e :<| SAtom lN (Int n) :<| Empty
     in case overloadedIntegerSymbol of
          Nothing -> SCombo l args
          Just sym -> SCombo l $ SAtom (loc e) (Sym sym) :<| args
  go (ZCombo l MakeFloat (e, lN, (sig, base, exp))) =
    let args = go e :<| SAtom lN (Int sig) :<| SAtom lN (Int $ fromIntegral base) :<| SAtom lN (Int exp) :<| Empty
     in case overloadedFloatSymbol of
          Nothing -> SCombo l args
          Just sym -> SCombo l $ SAtom (loc e) (Sym sym) :<| args
  go (ZCombo l MakeStr (e, lS, s)) =
    let args = go e :<| SAtom lS (Str s) :<| Empty
     in case overloadedStringSymbol of
          Nothing -> SCombo l args
          Just sym -> SCombo l $ SAtom (loc e) (Sym sym) :<| args
  go (ZCombo l MakeList (f, _, es)) =
    let args = go f :<| (go <$> es)
     in case overloadedListSymbol of
          Nothing -> SCombo l args
          Just sym -> SCombo l $ SAtom (loc f) (Sym sym) :<| args
  go (ZCombo l Tick (lQ, e)) =
    let op = SCombo l (SAtom lQ (Sym operativeSymbol) :<| ap :<| Empty)
        ap = SAtom lQ (Sym tickSymbol)
        args = Seq.singleton (go e)
     in if tickIsOperative conf
        then SCombo l (op :<| args)
        else SCombo l (ap :<| args)
  go (ZCombo l Backtick (lQ, e)) =
    let op = SCombo l (SAtom lQ (Sym operativeSymbol) :<| ap :<| Empty)
        ap = SAtom lQ (Sym backtickSymbol)
        args = Seq.singleton (go e)
     in if backtickIsOperative conf
        then SCombo l (op :<| args)
        else SCombo l (ap :<| args)
  go (ZCombo l Comma (lQ, e)) =
    let op = SCombo l (SAtom lQ (Sym operativeSymbol) :<| ap :<| Empty)
        ap = SAtom lQ (Sym commaSymbol)
        args = Seq.singleton (go e)
     in if commaIsOperative conf
        then SCombo l (op :<| args)
        else SCombo l (ap :<| args)
  go (ZCombo l CommaAt (lQ, e)) =
    let op = SCombo l (SAtom lQ (Sym operativeSymbol) :<| ap :<| Empty)
        ap = SAtom lQ (Sym commaAtSymbol)
        args = Seq.singleton (go e)
     in if commaAtIsOperative conf
        then SCombo l (op :<| args)
        else SCombo l (ap :<| args)
  operativeSymbol = intern (operativeName conf)
  qualifySymbol = intern (qualifyName conf)
  squareSymbol = intern (squareName conf)
  improperListSymbol = intern (improperListName conf)
  lensFieldSymbol = intern (lensFieldName conf)
  lensIndexSymbol = intern (lensIndexName conf)
  floatLiteralSymbol = intern (floatLiteralName conf)
  overloadedIntegerSymbol = intern <$> (overloadedIntegerOperativeName conf)
  overloadedFloatSymbol = intern <$> (overloadedFloatOperativeName conf)
  overloadedStringSymbol = intern <$> (overloadedStringOperativeName conf)
  overloadedListSymbol = intern <$> (overloadedListOperativeName conf)
  tickSymbol = intern (tickName conf)
  backtickSymbol = intern (backtickName conf)
  commaSymbol = intern (commaName conf)
  commaAtSymbol = intern (commaAtName conf)
