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

import Data.Functor ((<&>))
import Data.Sequence (Seq(..))
import Data.Symbol (Symbol,intern,unintern)
import Data.Zexpr.Location (Loc(..))
import Data.Zexpr.Sexpr (Atom(..),Sexpr(..))
import Data.Zexpr.Zexpr (Zexpr(..),Combine(..),loc)

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
  , overloadedIntegerName :: String
  , overloadedIntegerIsOperative :: Bool
  , overloadedFloatName :: String
  , overloadedFloatIsOperative :: Bool
  , overloadedStringName :: String
  , overloadedStringIsOperative :: Bool
  , overloadedListName :: String
  , overloadedListIsOperative :: Bool
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
  , overloadedIntegerName = "__mkInt__"
  , overloadedIntegerIsOperative = True
  , overloadedFloatName = "__mkFloat__"
  , overloadedFloatIsOperative = True
  , overloadedStringName = "__mkString__"
  , overloadedStringIsOperative = True
  , overloadedListName = "__mkList__"
  , overloadedListIsOperative = True
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
  go (ZCombo l Round (ZCombo dLoc Dollar op :<| opArgs)) =
    SCombo l (opSym dLoc :<| go op :<| (go <$> opArgs))
  go (ZCombo dLoc Dollar op) =
    SCombo (dLoc <> loc op) (opSym dLoc :<| go op :<| Empty)
  go (ZCombo l QualName ss) =
    let op = SAtom l (Sym qualifySymbol)
        args = ss <&> \(lS, s) -> SAtom lS (Sym s)
     in if qualifyIsOperative conf
        then SCombo l (opSym l :<| op :<| args)
        else SCombo l (op :<| args)
  go (ZCombo l Round es) = SCombo l (go <$> es)
  go (ZCombo l Square (lSq, es)) =
    let op = SAtom lSq $ Sym squareSymbol
        args = go <$> es
     in if squareIsOperative conf
        then SCombo l (opSym lSq :<| op :<| args)
        else SCombo l (op :<| args)
  go (ZCombo l ConsDot (es, lDot, e')) =
    let op = SAtom lDot $ Sym improperListSymbol
        args = (go <$> es) :|> go e'
     in if improperListIsOperative conf
        then SCombo l (opSym lDot :<| op :<| args)
        else SCombo l (op :<| args)
  go (ZCombo l LensField (e, lF, f)) =
    let op = SAtom lF (Sym lensFieldSymbol)
        fd = SAtom lF (Sym f)
     in if lensFieldIsOperative conf
        then SCombo l (opSym l :<| op :<| go e :<| fd :<| Empty)
        else SCombo l (op :<| go e :<| fd :<| Empty)
  go (ZCombo l LensIndex (e, ix)) =
    let op = SAtom (loc ix) (Sym lensIndexSymbol)
     in if lensIndexIsOperative conf
        then SCombo l (opSym (loc ix) :<| op :<| go e :<| go ix :<| Empty)
        else SCombo l (op :<| go e :<| go ix :<| Empty)
  go (ZCombo l FloatLit (sig, base, exp)) =
    let op = SAtom l (Sym floatLiteralSymbol)
        args = SAtom l (Int sig) :<| SAtom l (Int $ fromIntegral base) :<| SAtom l (Int exp) :<| Empty
     in if floatLiteralIsOperative conf
        then SCombo l (opSym l :<| op :<| args)
        else SCombo l (op :<| args)
  go (ZCombo l MakeInt (e, lN, n)) =
    let op = SAtom l (Sym overloadedIntegerSymbol)
        arg = SAtom lN (Int n)
     in if overloadedIntegerIsOperative conf
        then SCombo l (opSym l :<| op :<| go e :<| arg :<| Empty)
        else SCombo l (op :<| go e :<| arg :<| Empty)
  go (ZCombo l MakeFloat (e, lN, (sig, base, exp))) =
    let op = SAtom l (Sym overloadedFloatSymbol)
        args = SAtom lN (Int sig) :<| SAtom lN (Int $ fromIntegral base) :<| SAtom lN (Int exp) :<| Empty
     in if overloadedFloatIsOperative conf
        then SCombo l (opSym l :<| op :<| go e :<| args)
        else SCombo l (op :<| go e :<| args)
  go (ZCombo l MakeStr (e, lS, s)) =
    let op = SAtom l (Sym overloadedStringSymbol)
        arg = SAtom lS (Str s)
     in if overloadedStringIsOperative conf
        then SCombo l (opSym l :<| op :<| go e :<| arg :<| Empty)
        else SCombo l (op :<| go e :<| arg :<| Empty)
  go (ZCombo l MakeList (f, _, es)) =
    let op = SAtom l (Sym overloadedListSymbol)
        args = go <$> es
     in if overloadedListIsOperative conf
        then SCombo l (opSym l :<| op :<| go f :<| args)
        else SCombo l (op :<| go f :<| args)
  go (ZCombo l Tick (lQ, e)) =
    let op = SAtom lQ (Sym tickSymbol)
     in if tickIsOperative conf
        then SCombo l (opSym lQ :<| op :<| go e :<| Empty)
        else SCombo l (op :<| go e :<| Empty)
  go (ZCombo l Backtick (lQ, e)) =
    let op = SAtom lQ (Sym backtickSymbol)
     in if backtickIsOperative conf
        then SCombo l (opSym lQ :<| op :<| go e :<| Empty)
        else SCombo l (op :<| go e :<| Empty)
  go (ZCombo l Comma (lQ, e)) =
    let op = SAtom lQ (Sym commaSymbol)
     in if commaIsOperative conf
        then SCombo l (opSym lQ :<| op :<| go e :<| Empty)
        else SCombo l (op :<| go e :<| Empty)
  go (ZCombo l CommaAt (lQ, e)) =
    let op = SAtom lQ (Sym commaAtSymbol)
     in if commaAtIsOperative conf
        then SCombo l (opSym lQ :<| op :<| go e :<| Empty)
        else SCombo l (op :<| go e :<| Empty)
  opSym l = SAtom l (Sym operativeSymbol)
  operativeSymbol = intern (operativeName conf)
  qualifySymbol = intern (qualifyName conf)
  squareSymbol = intern (squareName conf)
  improperListSymbol = intern (improperListName conf)
  lensFieldSymbol = intern (lensFieldName conf)
  lensIndexSymbol = intern (lensIndexName conf)
  floatLiteralSymbol = intern (floatLiteralName conf)
  overloadedIntegerSymbol = intern (overloadedIntegerName conf)
  overloadedFloatSymbol = intern (overloadedFloatName conf)
  overloadedStringSymbol = intern (overloadedStringName conf)
  overloadedListSymbol = intern (overloadedListName conf)
  tickSymbol = intern (tickName conf)
  backtickSymbol = intern (backtickName conf)
  commaSymbol = intern (commaName conf)
  commaAtSymbol = intern (commaAtName conf)
