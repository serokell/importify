{-# LANGUAGE ApplicativeDo #-}

-- | Command line options for Importify

module Options
       ( ImportifyCliArgs  (..)
       , CommonOptions     (..)
       , Command (..)
       , CabalCacheOptions (..)
       , SingleFileOptions (..)

       , parseOptions
       ) where

import           Universum

import           Options.Applicative (Parser, ParserInfo, auto, command, execParser,
                                      flag', fullDesc, help, helper, info, long, metavar,
                                      option, progDesc, short, showDefault, strArgument,
                                      strOption, subparser, switch, value)
import qualified Prelude             (show)
import           System.Wlog         (Severity (Info))

import           Importify.Main      (OutputOptions (..))

data ImportifyCliArgs = ImportifyCliArgs
    { icaCommon  :: !CommonOptions
    , icaCommand :: !Command
    } deriving (Show)

data CommonOptions = CommonOptions
    { coLoggingSeverity :: !Severity -- ^ Severity for logger
    } deriving (Show)

data Command
    = CabalCache   CabalCacheOptions
    | RemoveUnused SingleFileOptions
    | ToExplicit   SingleFileOptions
    deriving (Show)

data SingleFileOptions = SingleFileOptions
    { sfoFileName :: !FilePath                    -- ^ File that should be processed
    , sfoOutput   :: !(FilePath -> OutputOptions) -- ^ Describes way of @importify@ output
    }

instance Show SingleFileOptions where
    show SingleFileOptions{..} = "SingleFileOptions{ sfoFileName = " ++ sfoFileName ++ "\n"
                              ++ "                 , sfoOutput X = " ++ show (sfoOutput "X") ++ " }"

data CabalCacheOptions = CabalCacheOptions
    { ccoSaveSources  :: !Bool    -- ^ Don't delete downloaded package cache
    , ccoDependencies :: ![Text]  -- ^ Use specified dependencies overriding .cabal ones
    } deriving (Show)

cliArgumentsParser :: Parser ImportifyCliArgs
cliArgumentsParser = do
    icaCommon  <- commonArgsParser
    icaCommand <- commandParser
    pure ImportifyCliArgs{..}

commonArgsParser :: Parser CommonOptions
commonArgsParser = do
    coLoggingSeverity <- option auto $
        metavar "SEVERITY"
     <> long "logging-severity"
     <> short 'l'
     <> value Info
     <> showDefault
     <> help "Severity for logger"
    pure CommonOptions{..}

commandParser :: Parser Command
commandParser = subparser $
    command "cache"
            (info (helper <*> cacheParser)
                  (fullDesc <> progDesc
                   "Search for .cabal file in current directory. If it's found then cache \
                   \all dependencies for every target and store them inside ./.importify folder."))
 <> command "remove"
            (info (helper <*> fmap RemoveUnused singleFileOptionsParser)
                  (fullDesc <> progDesc "Remove unused imports from file."))
 <> command "to-explicit"
            (info (helper <*> fmap ToExplicit singleFileOptionsParser)
                  (fullDesc <> progDesc "Convert all implicit imports to explicit."))

singleFileOptionsParser :: Parser SingleFileOptions
singleFileOptionsParser = do
    sfoFileName <- strArgument
      $ metavar "FILE_PATH"
     <> help "File to importify"
    sfoOutput <- outputOptionsParser
    pure SingleFileOptions{..}
  where
    outputOptionsParser :: Parser (FilePath -> OutputOptions)
    outputOptionsParser =
         flag' ToFile
               (  long "in-place"
               <> short 'i'
               <> help "Write changes directly to file")
     <|> const . ToFile <$> strOption
         (  long "to"
         <> short 't'
         <> metavar "FILE_PATH"
         <> help "Write to specified file")
     <|> pure (const ToConsole)

cacheParser :: Parser Command
cacheParser = do
    ccoSaveSources <- switch
      $ long "preserve"
     <> short 'p'
     <> help "Don't remove downloaded package cache"
    ccoDependencies <- many $ strOption
      $ metavar "[STRING]"
     <> long "dependency"
     <> short 'd'
     <> help "List of manually specified dependencies that should be used \
             \for caching instead of libraries from .cabal file. This option \
             \overrides default behavior."
    pure (CabalCache CabalCacheOptions{..})

optsInfo :: ParserInfo ImportifyCliArgs
optsInfo = info
    (helper <*> cliArgumentsParser)
    (fullDesc <> progDesc "Importify - manage Haskell imports easily")

parseOptions :: IO ImportifyCliArgs
parseOptions = execParser optsInfo
