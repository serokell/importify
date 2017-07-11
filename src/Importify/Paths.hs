{-# LANGUAGE QuasiQuotes #-}

-- | This module contains common utilities for working with importify cache.

module Importify.Paths
       ( -- * Predefined directories
         cacheDir
       , cachePath
       , extensionsFile
       , extensionsPath
       , modulesFile
       , modulesPath
       , symbolsDir
       , symbolsPath
       , targetsFile
       , targetsPath
       , testDataPath
       , testDataDir

         -- * Utility functions to work with directories
       , doInsideDir
       , guessCabalName
       ) where

import           Universum

import           Path             (Abs, Dir, File, Rel, fromAbsDir, fromRelDir,
                                   fromRelFile, reldir, relfile)
import           Path.Internal    (Path (..))
import           System.Directory (createDirectoryIfMissing)
import           System.FilePath  ((<.>))
import           Turtle           (cd, pwd)

cachePath :: Path Rel Dir
cachePath = [reldir|.importify/|]

-- | Path to file that stores mapping from module names to their packages.
modulesPath :: Path Rel File
modulesPath = [relfile|modules|]

symbolsPath :: Path Rel Dir
symbolsPath = [reldir|symbols/|]

-- | Path to JSON-encoded Map from project module name to
-- its target (i.e. __library__, __executable__).
targetsPath :: Path Rel File
targetsPath = [relfile|targets|]

-- | Path to golden tests.
testDataPath :: Path Rel Dir
testDataPath = [reldir|test/test-data/|]

-- | Path to JSON-encoded Map from target to its list of default extensions.
extensionsPath :: Path Rel File
extensionsPath = [relfile|extensions|]

cacheDir, extensionsFile, modulesFile, symbolsDir, targetsFile, testDataDir :: FilePath
cacheDir       = fromRelDir  cachePath
extensionsFile = fromRelFile extensionsPath
modulesFile    = fromRelFile modulesPath
symbolsDir     = fromRelDir  symbolsPath
targetsFile    = fromRelFile targetsPath
testDataDir    = fromRelDir  testDataPath

-- TODO: probably not reliable, instead file with
-- .cabal extension should be searched
guessCabalName :: FilePath -> Path Rel File
guessCabalName libName = Path $ libName <.> ".cabal"

createCacheDir :: Path Abs Dir -> IO ()
createCacheDir importifyPath = do
    let importifyDir = fromAbsDir importifyPath
    createDirectoryIfMissing True importifyDir -- creates ./.importify

-- | Create given directory and perform given action inside it.
doInsideDir :: Path Abs Dir -> IO () -> IO ()
doInsideDir dir action = do
    thisDirectory <- pwd
    bracket_ (do createCacheDir dir
                 cd $ fromString $ fromAbsDir dir)
             (cd thisDirectory)
             action
