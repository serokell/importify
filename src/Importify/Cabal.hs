-- | This module contains common utilities for parsing .cabal files
-- and manipulating it's AST.

module Importify.Cabal
       ( getLibs
       , modulePaths
       , readCabal
       , withLibrary
       ) where

import           Universum

import           Distribution.ModuleName               (ModuleName, toFilePath)
import           Distribution.Package                  (Dependency (..), PackageName (..))
import           Distribution.PackageDescription       (BuildInfo (..), CondTree,
                                                        GenericPackageDescription (..),
                                                        Library (..), condTreeData)
import           Distribution.PackageDescription.Parse (readPackageDescription)
import           Distribution.Verbosity                (normal)
import           Path                                  (Abs, Dir, File, Path, Rel,
                                                        fromRelFile, parseRelDir,
                                                        parseRelFile, (</>))
import           System.Directory                      (doesFileExist)

readCabal :: FilePath -> IO GenericPackageDescription
readCabal = readPackageDescription normal

getNodeLibInfo :: CondTree var deps Library -> BuildInfo
getNodeLibInfo = libBuildInfo . condTreeData

-- TODO: also collect for executables and make unique
-- TODO: what about version bounds?
-- | Retrieve list of all package dependencies for library of
-- given package.
getLibs :: GenericPackageDescription -> [String]
getLibs GenericPackageDescription{..} =
    maybe []
          (map dependencyName . targetBuildDepends . getNodeLibInfo)
          condLibrary

dependencyName :: Dependency -> String
dependencyName (Dependency PackageName{..} _) = unPackageName

-- | Perform given action with package library 'BuilInfo'
-- if 'Library' is present. We care only about library exposed modules
-- because only they can be imported outside that package.
withLibrary :: Applicative f
            => GenericPackageDescription
            -> (Library -> f ())
            -> f ()
withLibrary GenericPackageDescription{..} action =
    whenJust condLibrary (action . condTreeData)

-- | Returns list of relative paths to each module.
modulePaths :: Path Rel Dir -> Library -> IO [Path Rel File]
modulePaths packagePath Library{..} = do
    sourcePaths <- mapM parseRelDir $ hsSourceDirs libBuildInfo
    collectModulePaths sourcePaths
  where
    exposedModulesPaths :: IO [Path Rel File]
    exposedModulesPaths = mapM (parseRelFile . (++ ".hs") . toFilePath) exposedModules

    addAbsDir :: Path Rel Dir -> [Path Rel File] -> [Path Rel File]
    addAbsDir dir = map (dir </>)

    collectModulePaths :: [Path Rel Dir] -> IO [Path Rel File]
    collectModulePaths []   = exposedModulesPaths >>=
        checkModuleExistence . addAbsDir packagePath
    collectModulePaths dirs =
        exposedModulesPaths  >>= \paths ->
        concatForM dirs        $ \dir   ->
          checkModuleExistence $ addAbsDir (packagePath </> dir) paths

    -- TODO: rewrite with either mapM or filterM
    checkModuleExistence :: [Path Rel File] -> IO [Path Rel File]
    checkModuleExistence []     = return []
    checkModuleExistence (modPath:modPaths) = do
        modExists  <- doesFileExist $ fromRelFile modPath
        if modExists
        then (modPath:) <$> checkModuleExistence modPaths
        else checkModuleExistence modPaths
