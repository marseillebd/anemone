{-# LANGUAGE LambdaCase #-}

module Main (main) where

import Data.Functor ((<&>))
import Data.Symbol (intern)
import Data.Text.Prettyprint.Doc (Doc,Pretty(pretty))
import Data.Text.Prettyprint.Doc.Render.Text (renderIO)
import Data.Text.Prettyprint.Doc.Util (putDocW)
import Data.Zexpr (toSexpr,Conf(..),defaultConf)
import Data.Zexpr.Sexpr.Text.Render (renderPretty)
import Data.Zexpr.Text.Parser (parse,errorBundlePretty)
import Language.Bslisp.TreeWalk.Environment (newDefaultEnv,Env(..))
import Language.Bslisp.TreeWalk.Eval (eval)
import Language.Bslisp.TreeWalk.Stack (makeTrace)
import System.Exit (exitFailure)
import System.IO (Handle,stderr)
import System.IO (stderr,hPutStr,hPutStrLn)
import Text.Pretty.Simple (pPrint)

import qualified Data.Text.IO as T
import qualified Data.Text.Prettyprint.Doc as PP


main :: IO ()
main = anemoneMain

anemoneMain :: IO ()
anemoneMain = do
  inp <- T.getContents
  vs <- case parse "<stdin>" inp of
    Left err -> do
      hPutStr stderr $ errorBundlePretty err
      exitFailure
    Right vs -> pure vs
  ((>> putStrLn "") . putDocW 100 . renderPretty . toSexpr defaultConf) `mapM_` vs
  env0 <- newDefaultEnv <&> \e -> e{name=Just (intern "__top__")}
  eval (toSexpr defaultConf <$> vs) env0 >>= \case
    Right v -> pPrint v
    Left exn -> do
      pPrint exn
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
  , overloadedIntegerOperativeName = Just "is-int"
  , overloadedFloatOperativeName = Just "is-float"
  , overloadedStringOperativeName = Just "is-string"
  , overloadedListOperativeName = Just "is-list"
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
