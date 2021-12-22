module Main (main) where

import Data.Zexpr (toSexpr,defaultConf)
import Data.Zexpr.Sexpr.Text.Render (renderPlain)
import Data.Zexpr.Text.Parser (parse,errorBundlePretty)
import System.Exit (exitFailure)
import System.IO (stderr,hPutStr)

import qualified Data.Text.IO as T


main :: IO ()
main = do
  inp <- T.getContents
  vs <- case parse "<stdin>" inp of
    Left err -> do
      hPutStr stderr $ errorBundlePretty err
      exitFailure
    Right vs -> pure vs
  (putStrLn . renderPlain . toSexpr defaultConf) `mapM_` vs
