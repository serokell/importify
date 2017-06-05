{-# LANGUAGE QuasiQuotes #-}

-- | This module contains common utilities for working with importify cache.

module Importify.Cache
       ( -- * Predefined directories
         cacheDir
       , cachePath
--       , libsDir
       , symbolsDir
       , symbolsPath

         -- * Utility functions to work with directories
       , guessCabalName
       ) where

import           Universum

import           Path            (Dir, File, Rel, fromRelDir, reldir)
import           Path.Internal   (Path (..))
import           System.FilePath ((<.>))

cachePath :: Path Rel Dir
cachePath = [reldir|.importify/|]

-- TODO: do we need to store downloaded sources? Probably not.
-- libsPath :: Path Rel Dir
-- libsPath = importifyCachePath </> [reldir|libs|]

symbolsPath :: Path Rel Dir
symbolsPath = [reldir|symbols|]

cacheDir, symbolsDir :: FilePath
cacheDir   = fromRelDir cachePath
-- libsDir    = toFilePath libsPath
symbolsDir = fromRelDir symbolsPath

-- TODO: probably not reliable, instead file with
-- .cabal extension should be searched
guessCabalName :: FilePath -> Path Rel File
guessCabalName libName = Path $ libName <.> ".cabal"
