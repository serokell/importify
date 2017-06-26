{-# LANGUAGE QuasiQuotes #-}

-- | This module contains common utilities for working with importify cache.

module Importify.Cache
       ( -- * Predefined directories
         cacheDir
       , cachePath
       , modulesFile
       , modulesPath
       , symbolsDir
       , symbolsPath

         -- * Utility functions to work with directories
       , guessCabalName
       ) where

import           Universum

import           Path            (Dir, File, Rel, fromRelDir, fromRelFile, reldir,
                                  relfile)
import           Path.Internal   (Path (..))
import           System.FilePath ((<.>))

cachePath :: Path Rel Dir
cachePath = [reldir|.importify/|]

symbolsPath :: Path Rel Dir
symbolsPath = [reldir|symbols/|]

-- | Path to file that stores mapping from module names to their packages.
modulesPath :: Path Rel File
modulesPath = [relfile|modules|]

cacheDir, modulesFile, symbolsDir :: FilePath
cacheDir    = fromRelDir  cachePath
modulesFile = fromRelFile modulesPath
symbolsDir  = fromRelDir  symbolsPath

-- TODO: probably not reliable, instead file with
-- .cabal extension should be searched
guessCabalName :: FilePath -> Path Rel File
guessCabalName libName = Path $ libName <.> ".cabal"
