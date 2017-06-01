-- | This module contains common utilities for parsing .cabal files
-- and manipulating it's AST.

module Importify.Cabal
       ( getLibs
       , readCabal
       ) where

import           Universum

import           Distribution.Package                  (Dependency (..), PackageName (..))
import           Distribution.PackageDescription       (GenericPackageDescription (..),
                                                        condTreeData, libBuildInfo,
                                                        targetBuildDepends)
import           Distribution.PackageDescription.Parse (readPackageDescription)
import           Distribution.Verbosity                (normal)

readCabal :: FilePath -> IO GenericPackageDescription
readCabal = readPackageDescription normal

-- TODO: also collect for executables and make unique
-- TODO: what about version bounds?
getLibs :: GenericPackageDescription -> [String]
getLibs GenericPackageDescription{..} =
    maybe []
          (map dependencyName . targetBuildDepends . libBuildInfo . condTreeData)
          condLibrary

dependencyName :: Dependency -> String
dependencyName (Dependency PackageName{..} _) = unPackageName
