{-# LANGUAGE ApplicativeDo #-}

-- | Command line options for Importify

module Options
       ( Command (..)

       , CabalCacheOptions (..)
       , SingleFileOptions (..)

       , parseOptions
       ) where

import           Universum

import           Options.Applicative (Parser, ParserInfo, command, execParser, flag',
                                      fullDesc, help, helper, info, long, metavar,
                                      progDesc, short, strArgument, strOption, subparser,
                                      switch)

import           Importify.Main      (OutputOptions (..))

data Command
    = SingleFile SingleFileOptions
    | CabalCache CabalCacheOptions
    deriving (Show)

data SingleFileOptions = SingleFileOptions
    { sfoFilename :: !FilePath      -- ^ File to apply the tool to
    , sfoOutput   :: !OutputOptions -- ^ Options for @importify file@ output
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
    sfoOutput <- outputOptionsParser
    pure $ SingleFile SingleFileOptions{..}
  where
    outputOptionsParser :: Parser OutputOptions
    outputOptionsParser =
         flag' InPlace (  long "in-place"
                       <> short 'i'
                       <> help "Write changes directly to file")
     <|> ToFile <$> strOption (  long "to"
                              <> short 't'
                              <> help "Write to specified file")
     <|> pure ToConsole

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
