{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}

module Language.Bslisp.TreeWalk.Stack
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
import Data.Zexpr.Sexpr.Text.Render (renderSymbol)
import Language.Bslisp.TreeWalk.Unsafe.Types (StackItem(..),PushPop(..),ReturnFrom(..))
import Language.Bslisp.TreeWalk.Unsafe.Types (StackTrace(..),TraceItem(..))
import Language.Bslisp.TreeWalk.Value (Closure(..))
import Language.Bslisp.TreeWalk.Value (Control(..))

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
pop (Stack ((Operate loc sexprs) : ks)) = (Just $ Operate loc sexprs, Stack ks)
pop (Stack ((Args calledAt args) : ks)) = (Just $ Args calledAt args, Stack ks)
pop (Stack ((ArgVals calledAt (arg:|[])) : ks)) = (Just $ ArgVal calledAt arg, Stack ks)
pop (Stack ((ArgVals calledAt (arg:|(v:vs))) : ks)) = (Just $ ArgVal calledAt arg, Stack $ ArgVals calledAt (v:|vs) : ks)
pop (Stack ((ArgVal calledAt v) : ks)) = (Just $ ArgVal calledAt v, Stack ks)
pop (Stack ((Apply calledAt f []) : ks)) = (Just $ Apply1 calledAt f, Stack ks)
pop (Stack (Apply calledAt f [arg] : ks)) = (Just $ Apply1 calledAt f, Stack $ Args calledAt (arg:|[]) : ks)
pop (Stack (Apply calledAt f (arg:args) : ks)) = (Just $ Apply1 calledAt f, Stack $ Args calledAt (arg:|args) : ks)
pop (Stack (Restore from env : ks)) = (Just $ Restore from env, Stack ks)
pop (Stack (Sequence (stmt:|stmts) : ks)) = case stmts of
  [] -> (Just $ Then stmt, Stack ks)
  (s:ss) -> (Just $ Then stmt, Stack (Sequence (s:|ss) : ks))
pop (Stack ((OpDefine env loc x) : ks)) = (Just $ OpDefine env loc x, Stack ks)
pop (Stack ((OpList vs es) : ks)) = (Just $ OpList vs es, Stack ks)
pop (Stack ((PrimArg n f args) : ks)) = (Just $ PrimArg n f args, Stack ks)


unsafePop :: Stack -> (Maybe (StackItem 'Push), Stack)
unsafePop (Stack (k:ks)) = (Just k, Stack ks)
unsafePop st@(Stack []) = (Nothing, st)

toPush :: StackItem either -> StackItem 'Push
toPush (Operate loc sexprs) = Operate loc sexprs
toPush (Args calledAt sexpr) = Args calledAt sexpr
toPush (ArgVals calledAt vs) = ArgVals calledAt vs
toPush (ArgVal calledAt v) = ArgVal calledAt v
toPush (Apply calledAt f args) = Apply calledAt f args
toPush (Apply1 calledAt f) = Apply calledAt f []
toPush (Restore from env) = Restore from env
toPush (Sequence nexts) = Sequence nexts
toPush (Then next) = Sequence (next :| [])
toPush (OpDefine env loc x) = OpDefine env loc x
toPush (OpList vs sexprs) = OpList vs sexprs
toPush (PrimArg n f args) = PrimArg n f args

makeTrace :: Control -> StackTrace
makeTrace (PrimCtrl stack exn) = StackTrace (RList.catMaybes $ go <$> stack) exn
  where
  go (Operate _ _) = Nothing
  go (Args _ _) = Nothing
  go (ArgVals _ _) = Nothing
  go (ArgVal _ _) = Nothing
  go (Apply _ _ _) = Nothing
  go (Apply1 _ _) = Nothing
  go (Restore from env) = Just $ case from of
    FromCall{calledAt,callee,args} -> CallTrace
      { callerEnv = env
      , calledAt
      , callee
      , args
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
  go (Sequence _) = Nothing
  go (Then _) = Nothing
  go (OpDefine _ _ _) = Nothing
  go (OpList _ _) = Nothing
  go (PrimArg n f args) = Just $ PrimArgTrace n f args

instance Pretty StackTrace where
  pretty (StackTrace stack exn) = PP.vsep $ RList.reverse (goItem <$> stack) ++ [PP.viaShow exn]
    where
    goItem CallTrace{callee=Closure{name,definedAt},calledAt,args} =
      let nameInfo = case name of
            Just x -> "function " <> PP.pretty (renderSymbol x)
            Nothing -> "anonymous function"
          defLocInfo =  "(" <> pretty definedAt <> ")"
          callSiteInfo = "called at" <+> pretty calledAt
          argsInfo = PP.group . PP.nest 2 . PP.vsep $ "with arguments" : fmap pretty args
       in "in" <+> nameInfo <+> defLocInfo <+> callSiteInfo <+> argsInfo
    goItem EvalTrace{evaledAt,evaleeEnv,evalee} = PP.nest 2 . PP.vsep $
      [ "when eval'ing" <+> "(" <> pretty evaledAt <> ") in <" <> pretty evaleeEnv <> "> the s-expr:"
      , Sexpr.renderPretty evalee
      ]
    goItem ThunkTrace{forcedAt,thunkee} =
      let header = "while forcing thunk (" <> pretty forcedAt <> ") suspended from " <> pretty (Sexpr.loc thunkee)
       in header <> PP.hardline <> PP.indent 2 (Sexpr.renderPretty thunkee)
    goItem PrimArgTrace{} = "in argument <> of primitive <> with arguments <>"
