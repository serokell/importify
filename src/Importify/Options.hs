{-# LANGUAGE ApplicativeDo #-}

-- | Command line options for Importify

module Importify.Options
       ( Command(..)
       , SingleFileOptions(..)
       , parseOptions
       ) where

import           Universum

import           Options.Applicative (Parser, ParserInfo, argument, command, execParser,
                                      fullDesc, help, helper, info, metavar, progDesc,
                                      str, subparser)

data Command
    = SingleFile SingleFileOptions

data SingleFileOptions = SingleFileOptions
    { sfoFilename   :: !FilePath
    -- ^ File to apply to tool to
    }

optionsParser :: Parser Command
optionsParser =
    subparser
        (command
            "file"
            (info (helper <*> singleParser)
                  (fullDesc <> progDesc "Importify a single file"))) <|>
    singleParser


singleParser :: Parser Command
singleParser = do
    sfoFilename <- argument str $
        metavar "FILE" <>
        help "File to importify"
    return $ SingleFile SingleFileOptions{..}

optsInfo :: ParserInfo Command
optsInfo = info
    (helper <*> optionsParser)
    (fullDesc <> progDesc "Importify - manage Haskell imports easily")

parseOptions :: IO Command
parseOptions = execParser optsInfo
