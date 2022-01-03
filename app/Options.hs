{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE RecordWildCards #-}

module Options
  ( Options(..)
  , getOptions
  , options
  ) where

import Options.Applicative

data Options
  = Version { numeric :: !Bool }
  | Run
    { inputFilepaths :: ![FilePath]
    , interactive :: !Bool
    }

getOptions :: IO Options
getOptions = execParser options

options :: ParserInfo Options
options = info (optionParser <**> helper)
  $  fullDesc
  <> progDesc "interpreter for Anemone programs"

optionParser :: Parser Options
optionParser = do
  version <- switch
    $  long "version"
    <> help "Show human-redable version (for a stable format, see --numeric-version)"
  numericVersion <- switch
    $  long "numeric-version"
    <> help "Show version (stable format, for use in scripts)"
  inputFilepaths <- many $ strArgument
    $  metavar "FILES..."
    <> help "evaluate these files in order"
  interactive <- switch
    $  long "interactive"
    <> short 'i'
    <> help "begin a repl (after evaluating any input files)"
  pure $
    if| numericVersion -> Version{numeric = True}
      | version -> Version{numeric = False}
      | otherwise -> Run{..}
