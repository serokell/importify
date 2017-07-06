{-| Tool for managing import sections.

    Remove redundant imports algorithm (current version):
      1. For every import declaration that in @loadEnvironment@
         traverse list of import names and collect those that are not in module.
      2. Remove every name from corresponding imports lists.
      3. Print new modified version of file with imports changed.
 -}

module Main where

import           Universum

import           Importify.Main (doCache, doFile)

import           Options        (CabalCacheOptions (..), Command (..),
                                 SingleFileOptions (..), parseOptions)

main :: IO ()
main = do
    opts <- parseOptions
    case opts of
        SingleFile sfOpts -> importifySingleFile sfOpts
        CabalCache ccOpts -> buildCabalCache ccOpts

importifySingleFile :: SingleFileOptions -> IO ()
importifySingleFile SingleFileOptions{..} =
    doFile sfoInPlace sfoFilename

buildCabalCache :: CabalCacheOptions -> IO ()
buildCabalCache CabalCacheOptions{..} =
    doCache ccoFilename ccoPreserve ccoDependencies
