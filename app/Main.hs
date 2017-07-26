{-| Tool for managing import sections.

    Remove redundant imports algorithm (current version):
      1. For every import declaration that in @loadEnvironment@
         traverse list of import names and collect those that are not in module.
      2. Remove every name from corresponding imports lists.
      3. Print new modified version of file with imports changed.
 -}

module Main where

import           Universum

import           Extended.System.Wlog (initImportifyLogger)
import           Importify.Main       (doCache, doFile)

import           Options              (CabalCacheOptions (..), Command (..),
                                       ImportifyCliArgs (..), SingleFileOptions (..),
                                       coLoggingSeverity, parseOptions)

main :: IO ()
main = do
    ImportifyCliArgs{..} <- parseOptions
    initImportifyLogger (coLoggingSeverity icaCommon)
    case icaCommand of
        SingleFile sfOpts -> importifySingleFile sfOpts
        CabalCache ccOpts -> buildCabalCache ccOpts

importifySingleFile :: SingleFileOptions -> IO ()
importifySingleFile SingleFileOptions{..} =
    doFile sfoOutput sfoFileName

buildCabalCache :: CabalCacheOptions -> IO ()
buildCabalCache CabalCacheOptions{..} =
    doCache ccoPreserve ccoDependencies
