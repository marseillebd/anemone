module Main (main) where

import Data.Zexpr (crapDisplay)
import Data.Zexpr.Parser (parse)
import System.Exit (exitFailure)
import System.IO (stderr,hPutStr)

import qualified Data.Text.IO as T
import qualified Text.Megaparsec as MP


main :: IO ()
main = do
  inp <- T.getContents
  vs <- case parse "<stdin>" inp of
    Left err -> do
      hPutStr stderr $ MP.errorBundlePretty err
      exitFailure
    Right vs -> pure vs
  (putStrLn . crapDisplay) `mapM_` vs
