{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}

{-# OPTIONS_GHC -fno-warn-unused-top-binds #-}

module Main (main) where

import Control.Monad (forM)
import Data.Functor ((<&>))
import Data.Text.Prettyprint.Doc (Pretty(pretty))
import Data.Text.Prettyprint.Doc.Render.Text (renderIO)
import Data.Text.Prettyprint.Doc.Util (putDocW)
import Data.Zexpr (toSexpr,Conf(..),defaultConf)
import Data.Zexpr.Sexpr.Text.Render (renderPretty)
import Data.Zexpr.Text.Parser (parse,errorBundlePretty)
import Language.Anemone.TreeWalk.Environment (Env(..))
import Language.Anemone.TreeWalk.Environment.Default (newDefaultEnv)
import Language.Anemone.TreeWalk.Eval (eval)
import Language.Anemone.TreeWalk.Stack (makeTrace)
import Options (Options(..),getOptions)
import System.Exit (exitSuccess,exitFailure)
import System.IO (Handle,stdout,stderr,hPutStr,hPutStrLn)
import Text.Pretty.Simple (pPrint)

import qualified Data.Text.IO as T
import qualified Data.Text.Prettyprint.Doc as PP


main :: IO ()
main = getOptions >>= \case
  Version{numeric} -> error $ "TODO: version " ++ show numeric
  Run{inputFilepaths,interactive} -> do
    zexprs <- (concat <$>) $ forM inputFilepaths $ \inputFilepath -> do
      inp <- T.readFile inputFilepath
      case parse inputFilepath inp of
        Left err -> do
          hPutStr stderr $ errorBundlePretty err
          exitFailure
        Right zexprs -> pure zexprs
    env0 <- newDefaultEnv <&> \e -> e{name=Nothing}
    eval (toSexpr defaultConf <$> zexprs) env0 >>= \case
      Right v -> hPutPrettyLn stdout v -- DEBUG
      Left exn -> do
        hPutPrettyLn stderr $ makeTrace exn
        exitFailure
    if interactive
      then error "TODO: repl"
      else exitSuccess

anemoneMain :: IO ()
anemoneMain = do
  inp <- T.getContents
  vs <- case parse "<stdin>" inp of
    Left err -> do
      hPutStr stderr $ errorBundlePretty err
      exitFailure
    Right vs -> pure vs
  -- ((>> putStrLn "") . putDocW 100 . renderPretty . toSexpr defaultConf) `mapM_` vs
  env0 <- newDefaultEnv <&> \e -> e{name=Nothing}
  eval (toSexpr defaultConf <$> vs) env0 >>= \case
    Right v -> do
      pPrint v
      hPutPrettyLn stdout v
    Left exn -> do
      -- pPrint exn
      hPutPrettyLn stderr $ makeTrace exn
      exitFailure

lispMain :: IO ()
lispMain = do
  inp <- T.getContents
  vs <- case parse "<stdin>" inp of
    Left err -> do
      hPutStr stderr $ errorBundlePretty err
      exitFailure
    Right vs -> pure vs
  ((>> putStrLn "") . putDocW 100 . renderPretty . toSexpr lispConf) `mapM_` vs

lispConf :: Conf
lispConf = Conf
  { operativeName = "__operate__"
  , qualifyName = "qname"
  , qualifyIsOperative = False
  , squareName = "list"
  , squareIsOperative = False
  , improperListName = "improper-list"
  , improperListIsOperative = False
  , lensFieldName = "lens-fld"
  , lensFieldIsOperative = False
  , lensIndexName = "lens-idx"
  , lensIndexIsOperative = False
  , floatLiteralName = "mk-float"
  , floatLiteralIsOperative = False
  , overloadedIntegerName = "is-int"
  , overloadedIntegerIsOperative = False
  , overloadedFloatName = "is-float"
  , overloadedFloatIsOperative = False
  , overloadedStringName = "is-string"
  , overloadedStringIsOperative = False
  , overloadedListName = "is-list"
  , overloadedListIsOperative = False
  , tickName = "quote"
  , tickIsOperative = False
  , backtickName = "quasiquote"
  , backtickIsOperative = False
  , commaName = "unquote"
  , commaIsOperative = False
  , commaAtName = "unquote-splicing"
  , commaAtIsOperative = False
  }

hPutPrettyLn :: (Pretty a) => Handle -> a -> IO ()
hPutPrettyLn handle doc = do
  renderIO handle $
    PP.layoutPretty PP.defaultLayoutOptions (PP.unAnnotate $ pretty doc)
  hPutStrLn handle ""
