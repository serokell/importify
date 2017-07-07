-- | Functions to operate with @exposed-modules@ and @other-modules@
-- parts of .cabal file.

module Importify.Cabal.Module
       ( modulePaths
       , splitOnExposedAndOther
       ) where

import           Universum                             hiding (fromString)

import qualified Data.HashMap.Strict                   as Map
import           Data.List                             (partition)
import           Distribution.ModuleName               (ModuleName, fromString,
                                                        toFilePath)
import qualified Distribution.ModuleName               as Cabal
import           Distribution.Package                  (Dependency (..), PackageName (..))
import           Distribution.PackageDescription       (BuildInfo (..), CondTree,
                                                        Executable (..),
                                                        GenericPackageDescription (..),
                                                        Library (..), condTreeData,
                                                        exeModules, libModules)
import           Distribution.PackageDescription.Parse (readPackageDescription)
import           Distribution.Verbosity                (normal)
import           Language.Haskell.Extension            (Extension (..),
                                                        KnownExtension (..))
import qualified Language.Haskell.Exts                 as HSE
import           Path                                  (Abs, Dir, File, Path, Rel,
                                                        fromAbsFile, parseRelDir,
                                                        parseRelFile, (</>))
import           System.Directory                      (doesFileExist)
import           System.FilePath.Posix                 (dropExtension)
import           Text.Read                             (read)

import           Importify.Syntax                      (getModuleTitle)

-- | Split list of modules into /exposed/ modules and /other/ modules for given library.
-- __First__ element of pair represents /exposed/ modules.
-- __Second__ element of paris represents /other/ modules.
splitOnExposedAndOther :: Library
                       -> [HSE.Module HSE.SrcSpanInfo]
                       -> ([HSE.Module HSE.SrcSpanInfo], [HSE.Module HSE.SrcSpanInfo])
splitOnExposedAndOther Library{..} =
    partition ((`elem` exposedModules) . Cabal.fromString . getModuleTitle)

-- | Returns list of absolute paths to both /exposed/ and /other/ modules.
modulePaths :: Path Abs Dir -> Library -> IO [Path Abs File]
modulePaths packagePath Library{..} = do
    let sourceDirs = hsSourceDirs libBuildInfo
    let (cur, others) = partition (== ".") sourceDirs
    case (cur, others) of
        (_here,    []) -> collectModulesHere
        ([]   , paths) -> collectModulesThere paths
        (_here, paths) -> liftA2 (++) collectModulesHere (collectModulesThere paths)
  where
    thisLibModules :: [ModuleName]
    thisLibModules = exposedModules ++ otherModules libBuildInfo

    libModulePaths :: IO [Path Rel File]
    libModulePaths = mapM (parseRelFile . (++ ".hs") . toFilePath) thisLibModules

    addDir :: Path Abs Dir -> [Path Rel File] -> [Path Abs File]
    addDir dir = map (dir </>)

    collectModulesHere :: IO [Path Abs File]
    collectModulesHere = do
        paths <- libModulePaths
        let packagePaths = addDir packagePath paths
        keepExistingModules packagePaths

    collectModulesThere :: [FilePath] -> IO [Path Abs File]
    collectModulesThere dirs = do
        dirPaths <- mapM parseRelDir dirs
        modPaths <- libModulePaths
        concatForM dirPaths $ \dir ->
          keepExistingModules $ addDir (packagePath </> dir) modPaths

    keepExistingModules :: [Path Abs File] -> IO [Path Abs File]
    keepExistingModules = filterM (doesFileExist . fromAbsFile)
