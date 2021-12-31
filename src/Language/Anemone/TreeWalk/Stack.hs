{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Language.Anemone.TreeWalk.Stack
  ( Stack
  , empty
  , StackItem(..)
  , PushPop(..)
  , push
  , pop
  , unsafePop
  , ReturnFrom(..)
  , toPush
  , makeTrace
  ) where

import Data.List.NonEmpty (NonEmpty(..))
import Data.Text.Prettyprint.Doc (Pretty(..),(<+>))
import Language.Anemone.TreeWalk.Unsafe.Types (StackItem(..),PushPop(..),ReturnFrom(..))
import Language.Anemone.TreeWalk.Unsafe.Types (StackTrace(..),TraceItem(..))
import Language.Anemone.TreeWalk.Value (Closure(..),Control(..),renderName)
import Language.Anemone.TreeWalk.Value (PrimExn(..))

import qualified Data.List.Reverse as RList
import qualified Data.Text.Prettyprint.Doc as PP
import qualified Data.Zexpr.Sexpr as Sexpr
import qualified Data.Zexpr.Sexpr.Text.Render as Sexpr

newtype Stack = Stack [StackItem 'Push]

empty :: Stack
empty = Stack []

push :: (StackItem 'Push) -> Stack -> Stack
push k (Stack ks) = Stack (k:ks)

pop :: Stack -> (Maybe (StackItem 'Pop), Stack)
pop st@(Stack []) = (Nothing, st)
pop (Stack ((Operate invokedAt loc sexprs) : ks)) =
  (Just $ Operate invokedAt loc sexprs, Stack ks)
pop (Stack ((Args calledAt fLoc args) : ks)) =
  (Just $ Args calledAt fLoc args, Stack ks)
pop (Stack ((ArgVals calledAt fLoc (arg:|[])) : ks)) =
  (Just $ ArgVal calledAt fLoc arg, Stack ks)
pop (Stack ((ArgVals calledAt fLoc (arg:|(v:vs))) : ks)) =
  (Just $ ArgVal calledAt fLoc arg, Stack $ ArgVals calledAt fLoc (v:|vs) : ks)
pop (Stack ((ArgVal calledAt fLoc v) : ks)) =
  (Just $ ArgVal calledAt fLoc v, Stack ks)
pop (Stack ((Apply calledAt (fLoc, f) argLoc []) : ks)) =
  (Just $ Apply1 calledAt (fLoc, f) argLoc, Stack ks)
pop (Stack (Apply calledAt (fLoc, f) argLoc [arg] : ks)) =
  (Just $ Apply1 calledAt (fLoc, f) argLoc, Stack $ Args calledAt (fLoc <> argLoc) (arg:|[]) : ks)
pop (Stack (Apply calledAt (fLoc, f) argLoc (arg:args) : ks)) =
  (Just $ Apply1 calledAt (fLoc, f) argLoc, Stack $ Args calledAt (fLoc <> argLoc) (arg:|args) : ks)
pop (Stack (Restore from env : ks)) =
  (Just $ Restore from env, Stack ks)
pop (Stack (Sequence stmtLoc (stmt:|stmts) : ks)) = case stmts of
  [] -> (Just $ Then stmtLoc stmt, Stack ks)
  (s:ss) -> (Just $ Then stmtLoc stmt, Stack (Sequence (Sexpr.loc s) (s:|ss) : ks))
pop (Stack (Cond pLoc c arcs : ks)) =
  (Just $ Cond pLoc c arcs, Stack ks)
pop (Stack ((OpDefineHere env loc x bodyLoc) : ks)) =
  (Just $ OpDefineHere env loc x bodyLoc, Stack ks)
pop (Stack ((OpList vs itemLoc es) : ks)) =
  (Just $ OpList vs itemLoc es, Stack ks)
pop (Stack ((PrimArg n calledAt f args) : ks)) =
  (Just $ PrimArg n calledAt f args, Stack ks)


unsafePop :: Stack -> (Maybe (StackItem 'Push), Stack)
unsafePop (Stack (k:ks)) = (Just k, Stack ks)
unsafePop st@(Stack []) = (Nothing, st)

toPush :: StackItem either -> StackItem 'Push
toPush (Operate invokedAt loc sexprs) = Operate invokedAt loc sexprs
toPush (Args calledAt fLoc sexpr) = Args calledAt fLoc sexpr
toPush (ArgVals calledAt fLoc vs) = ArgVals calledAt fLoc vs
toPush (ArgVal calledAt fLoc v) = ArgVal calledAt fLoc v
toPush (Apply calledAt f argLoc args) = Apply calledAt f argLoc args
toPush (Apply1 calledAt f argLoc) = Apply calledAt f argLoc []
toPush (Restore from env) = Restore from env
toPush (Sequence stmtLoc nexts) = Sequence stmtLoc nexts
toPush (Then stmtLoc next) = Sequence stmtLoc (next :| [])
toPush (Cond pLoc c arcs) = Cond pLoc c arcs
toPush (OpDefineHere env loc x bodyLoc) = OpDefineHere env loc x bodyLoc
toPush (OpList vs itemLoc sexprs) = OpList vs itemLoc sexprs
toPush (PrimArg n calledAt f args) = PrimArg n calledAt f args

makeTrace :: Control -> StackTrace
makeTrace (PrimCtrl stack exn) = StackTrace (RList.catMaybes $ go <$> stack) exn
  where
  go (Operate _ _ _) = Nothing
  go (Args _ _ _) = Nothing
  go (ArgVals _ _ _) = Nothing
  go (ArgVal _ _ _) = Nothing
  go (Apply _ _ _ _) = Nothing
  go (Apply1 _ _ _) = Nothing
  go (Restore from env) = Just $ case from of
    FromCall{calledAt,callee} -> CallTrace
      { callerEnv = env
      , calledAt
      , callee
      }
    FromEval{evaledAt,evaleeEnv,evalee} -> EvalTrace
      { evalerEnv = env
      , evaledAt
      , evaleeEnv
      , evalee
      }
    FromThunk{forcedAt,thunkeeEnv,thunkee} -> ThunkTrace
      { forcerEnv = env
      , forcedAt
      , thunkeeEnv
      , thunkee
      }
  go (Sequence _ _) = Nothing
  go (Then _ _) = Nothing
  go (Cond _ _ _) = Nothing
  go (OpDefineHere _ _ _ _) = Nothing
  go (OpList _ _ _) = Nothing
  go (PrimArg argNum calledAt primFunc _) = Just $ PrimArgTrace{argNum,calledAt,primFunc}

instance Pretty StackTrace where
  pretty (StackTrace stack (loc, exn)) = PP.vsep $ RList.reverse (goItem <$> stack) ++ [goExn]
    where
    goExn = PP.nest 2 . PP.vsep $ ["unhandled control raised from" <+> pretty loc, renderExn exn]
    goItem CallTrace{callee=Closure{name,definedAt},calledAt} =
      let nameInfo = case name of
            Just x -> "function " <> renderName x
            Nothing -> "anonymous function"
          defLocInfo =  "(" <> pretty definedAt <> ")"
          callSiteInfo = "called at" <+> pretty calledAt
       in "in" <+> nameInfo <+> defLocInfo <+> callSiteInfo
    goItem EvalTrace{evaledAt,evaleeEnv,evalee} = PP.nest 2 . PP.vsep $
      [ "when eval'ing" <+> "(" <> pretty evaledAt <> ") in <" <> pretty evaleeEnv <> "> the s-expr:"
      , Sexpr.renderPretty evalee
      ]
    goItem ThunkTrace{forcedAt,thunkee} =
      let header = "while forcing thunk (" <> pretty forcedAt <> ") suspended from " <> pretty (Sexpr.loc thunkee)
       in header <> PP.hardline <> PP.indent 2 (Sexpr.renderPretty thunkee)
    goItem PrimArgTrace{argNum,calledAt,primFunc} =
      "in argument" <+> pretty argNum <+> "of primitive" <+> PP.viaShow primFunc <+> "called at" <+> pretty calledAt

renderExn :: PrimExn -> PP.Doc ann
renderExn (ScopeErr env name) = "Scope Error:" <+> renderName name <+> "in" <+> "<" <> pretty env <> ">"
renderExn (SyntaxErr sexpr msg) = PP.nest 2 . PP.vsep $ ["Syntax Error:" <+> pretty msg, Sexpr.renderPretty sexpr]
renderExn (UncallableExn v) = "Uncallable: cannot call value" <+> pretty v
renderExn (TypeErr expected value) = PP.group . PP.nest 2 $ PP.vsep
  [ "Type Error:"
  , "expecting:" <+> PP.viaShow expected
  , "got value:" <+> pretty value
  ]
