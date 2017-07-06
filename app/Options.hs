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
                                      progDesc, short, str, strArgument, strOption,
                                      subparser, switch)

data Command
    = SingleFile SingleFileOptions
    | CabalCache CabalCacheOptions
    deriving (Show)

data SingleFileOptions = SingleFileOptions
    { sfoFilename :: !FilePath -- ^ File to apply the tool to
    , sfoInPlace  :: !Bool     -- ^ if 'True' then modify file in place
    } deriving (Show)

data CabalCacheOptions = CabalCacheOptions
    { ccoFilename     :: !FilePath -- ^ Path to .cabal file
    , ccoPreserve     :: !Bool     -- ^ Don't delete downloaded package cache
    , ccoDependencies :: ![String] -- ^ Use specified dependencies overriding .cabal ones
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
    sfoFilename <- strArgument $
        metavar "FILE" <>
        help "File to importify"
    sfoInPlace <- switch $
        long "in-place" <>
        short 'i' <>
        help "Write changes directly to file"
    pure $ SingleFile SingleFileOptions{..}

cacheParser :: Parser Command
cacheParser = do
    ccoFilename <- strArgument $
        metavar "FILE" <>
        help "Path to .cabal file"
    ccoPreserve <- switch $
        long "preserve" <>
        short 'p' <>
        help "Don't remove downloaded package cache"
    ccoDependencies <- many $ strOption $
        metavar "STRING" <>
        long "dependency" <>
        short 'd' <>
        help "List of manually specified dependencies that should be used instead of .cabal file"
    pure $ CabalCache CabalCacheOptions{..}

optsInfo :: ParserInfo Command
optsInfo = info
    (helper <*> optionsParser)
    (fullDesc <> progDesc "Importify - manage Haskell imports easily")

parseOptions :: IO Command
parseOptions = execParser optsInfo
