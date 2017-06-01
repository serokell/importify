{-# LANGUAGE QuasiQuotes #-}

-- | This module contains common utilities for working with importify cache.

module Importify.Cache
       ( cachePath
       ) where

import           Universum

import           Path      (Dir, Path, Rel, reldir, toFilePath)

importifyCachePath :: Path Rel Dir
importifyCachePath = [reldir|.importify/|]

-- | Relative path to importify cache director: @.importify@
cachePath :: FilePath
cachePath = toFilePath importifyCachePath
