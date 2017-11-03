{-| Tool for managing import sections.

    Remove redundant imports algorithm (current version):
      1. For every import declaration that in @loadEnvironment@
         traverse list of import names and collect those that are not in module.
      2. Remove every name from corresponding imports lists.
      3. Print new modified version of file with imports changed.
 -}

module Main where

import           Universum

import           Extended.System.Wlog  (initImportifyLogger)
import           Importify.Environment (runCache)
import           Importify.Main        (OutputOptions, importifyCacheList,
                                        importifyCacheProject, importifyRemoveWithOptions,
                                        importifyToExplicitWithOptions)

import           Options               (CabalCacheOptions (..), Command (..),
                                        ImportifyCliArgs (..), SingleFileOptions (..),
                                        coLoggingSeverity, parseOptions)

main :: IO ()
main = do
    ImportifyCliArgs{..} <- parseOptions
    initImportifyLogger (coLoggingSeverity icaCommon)
    case icaCommand of
        CabalCache   ccOpts -> buildCabalCache ccOpts
        RemoveUnused sfOpts -> runCommand importifyRemoveWithOptions     sfOpts
        ToExplicit   sfOpts -> runCommand importifyToExplicitWithOptions sfOpts

buildCabalCache :: CabalCacheOptions -> IO ()
buildCabalCache CabalCacheOptions{..} =
    runCache ccoSaveSources $ case ccoDependencies of
        []     -> importifyCacheProject
        (d:ds) -> importifyCacheList (d :| ds)

runCommand :: (OutputOptions -> FilePath -> IO ()) -> SingleFileOptions -> IO ()
runCommand command SingleFileOptions{..} = command (sfoOutput sfoFileName) sfoFileName
