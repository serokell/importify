-- | Functions to operate with @exposed-modules@ and @other-modules@
-- parts of .cabal file.

module Importify.Cabal.Module
       ( modulePaths
       , splitOnExposedAndOther
       ) where

import           Universum                       hiding (fromString)

import           Data.List                       (partition)
import           Distribution.ModuleName         (ModuleName, fromString, toFilePath)
import qualified Distribution.ModuleName         as Cabal
import           Distribution.PackageDescription (BuildInfo (..), Library (..))
import qualified Language.Haskell.Exts           as HSE
import           Path                            (Abs, Dir, File, Path, Rel, parseRelDir,
                                                  parseRelFile, (</>))
import           Path.IO                         (doesFileExist)

import           Importify.Syntax                (getModuleTitle)

-- | Split list of modules into /exposed/ modules and /other/ modules for given library.
-- __First__ element of pair represents /exposed/ modules.
-- __Second__ element of paris represents /other/ modules.
splitOnExposedAndOther :: Library
                       -> [HSE.Module l]
                       -> ([HSE.Module l], [HSE.Module l])
splitOnExposedAndOther Library{..} =
    partition ((`elem` exposedModules) . Cabal.fromString . getModuleTitle)

-- | Returns list of absolute paths to all modules inside given target.
modulePaths :: Path Abs Dir
            -- ^ Absolute path to project directory
            -> BuildInfo
            -- ^ 'BuildInfo' for given target
            -> Either [ModuleName] FilePath
            -- ^ Modules for Library and path for others
            -> IO [Path Abs File]
modulePaths packagePath BuildInfo{..} extra = do
    let (cur, others) = partition (== ".") hsSourceDirs
    case (cur, others) of
        (_here,    []) -> collectModulesHere
        ([]   , paths) -> collectModulesThere paths
        (_here, paths) -> liftA2 (++) collectModulesHere (collectModulesThere paths)
  where
    modulesToPaths :: [ModuleName] -> IO [Path Rel File]
    modulesToPaths = mapM (parseRelFile . (++ ".hs") . toFilePath)

    targetModulePaths :: IO [Path Rel File]
    targetModulePaths = case extra of
        Left modules -> modulesToPaths $ otherModules ++ modules
        Right path   -> liftA2 (:) (parseRelFile path) (modulesToPaths otherModules)

    addDir :: Path Abs Dir -> [Path Rel File] -> [Path Abs File]
    addDir dir = map (dir </>)

    collectModulesHere :: IO [Path Abs File]
    collectModulesHere = do
        paths <- targetModulePaths
        let packagePaths = addDir packagePath paths
        keepExistingModules packagePaths

    collectModulesThere :: [FilePath] -> IO [Path Abs File]
    collectModulesThere dirs = do
        dirPaths <- mapM parseRelDir dirs
        modPaths <- targetModulePaths
        concatForM dirPaths $ \dir ->
          keepExistingModules $ addDir (packagePath </> dir) modPaths

    keepExistingModules :: [Path Abs File] -> IO [Path Abs File]
    keepExistingModules = filterM doesFileExist
