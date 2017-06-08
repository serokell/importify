{-# LANGUAGE ApplicativeDo #-}

-- | Command line options for Importify

module Options
       ( Command (..)

       , CabalCacheOptions (..)
       , SingleFileOptions (..)

       , parseOptions
       ) where

import           Universum

import           Options.Applicative (Parser, ParserInfo, argument, command, execParser,
                                      fullDesc, help, helper, info, long, metavar,
                                      progDesc, short, str, subparser, switch)

data Command
    = SingleFile SingleFileOptions
    | CabalCache CabalCacheOptions
    deriving (Show)

data SingleFileOptions = SingleFileOptions
    { sfoFilename    :: !FilePath -- ^ File to apply the tool to
    , sfoPrintUnused :: !Bool     -- ^ Print unused Ids to stderr. For debug
    } deriving (Show)

data CabalCacheOptions = CabalCacheOptions
    { ccoFilename :: !FilePath -- ^ Path to .cabal file
    } deriving (Show)

optionsParser :: Parser Command
optionsParser = subparser $
    command "file" (info (helper <*> fileParser)
                          (fullDesc <> progDesc "Importify a single file"))
 <> command "cache" (info (helper <*> cacheParser)
                           (fullDesc <> progDesc
                              "Store cache from .cabal file in ./.importify folder"))

fileParser :: Parser Command
fileParser = do
    sfoFilename <- argument str $
        metavar "FILE" <>
        help "File to importify"
    sfoPrintUnused <- switch $
        long "print-unused" <>
        short 'U' <>
        help "Print unused Ids to stderr. \
             \Intended for debugging and testing purposes"
    pure $ SingleFile SingleFileOptions{..}

cacheParser :: Parser Command
cacheParser = do
    ccoFilename <- argument str $
        metavar "FILE" <>
        help "Path to .cabal file"
    pure $ CabalCache CabalCacheOptions{..}

optsInfo :: ParserInfo Command
optsInfo = info
    (helper <*> optionsParser)
    (fullDesc <> progDesc "Importify - manage Haskell imports easily")

parseOptions :: IO Command
parseOptions = execParser optsInfo
