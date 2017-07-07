{-# LANGUAGE TupleSections #-}

-- | Functions to retrieve and store mapping from modules to their
-- targets and extensions.

module Importify.Cabal.Target
       ( ExtensionsMap
       , TargetsMap
       , MapBundle
       , getExtensionMaps
       ) where

import           Universum                             hiding (fromString)

import qualified Data.HashMap.Strict                   as HM
import           Data.List                             (partition)
import           Distribution.ModuleName               (ModuleName, fromString,
                                                        toFilePath)
import qualified Distribution.ModuleName               as Cabal
import           Distribution.Package                  (Dependency (..), PackageName (..))
import           Distribution.PackageDescription       (Benchmark (benchmarkBuildInfo),
                                                        BuildInfo (..), CondTree,
                                                        Executable (buildInfo),
                                                        GenericPackageDescription (..),
                                                        Library (..),
                                                        TestSuite (testBuildInfo),
                                                        benchmarkModules, condTreeData,
                                                        exeModules, libModules,
                                                        testModules)
import           Distribution.PackageDescription.Parse (readPackageDescription)
import           Distribution.Text                     (display)
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

type    TargetsMap = HashMap String String
type ExtensionsMap = HashMap String [String]
type MapBundle     = (TargetsMap, ExtensionsMap)

data TargetId = LibraryId
              | ExecutableId String
              | TestSuiteId  String
              | BenchmarkId  String

cabalTargetId :: TargetId -> String
cabalTargetId LibraryId               = "library"
cabalTargetId (ExecutableId exeName)  = "executable " ++ exeName
cabalTargetId (TestSuiteId testName)  = "test-suite " ++ testName
cabalTargetId (BenchmarkId benchName) = "benchmark "  ++ benchName

getExtensionMaps :: GenericPackageDescription -> MapBundle
getExtensionMaps GenericPackageDescription{..} =
    ( HM.unions $ libTM : exeTMs ++ testTMs ++ benchTMs
    , HM.unions $ libEM : exeEMs ++ testEMs ++ benchEMs
    )
  where
    (libTM, libEM) =
        maybe mempty (collectLibraryMaps . condTreeData) condLibrary

    (exeTMs, exeEMs) =
        collectTargetsListMaps condExecutables
                               ExecutableId
                               (collectTargetMaps exeModules buildInfo)

    (testTMs, testEMs) =
        collectTargetsListMaps condTestSuites
                               TestSuiteId
                               (collectTargetMaps testModules testBuildInfo)

    (benchTMs, benchEMs) =
        collectTargetsListMaps condBenchmarks
                               BenchmarkId
                               (collectTargetMaps benchmarkModules benchmarkBuildInfo)

collectLibraryMaps :: Library -> MapBundle
collectLibraryMaps = collectTargetMaps libModules libBuildInfo LibraryId

collectTargetsListMaps :: [(String, CondTree v c target)]
                       -> (String -> TargetId)
                       -> (TargetId -> target -> MapBundle)
                       -> ([TargetsMap], [ExtensionsMap])
collectTargetsListMaps treeList idConstructor mapBundler = unzip $ do
    (name, condTree) <- treeList
    pure $ mapBundler (idConstructor name) $ condTreeData condTree

collectTargetMaps :: (target -> [ModuleName])
                  -> (target -> BuildInfo)
                  -> TargetId
                  -> target
                  -> MapBundle
collectTargetMaps modulesExtractor buildInfoExtractor id target =
    collectModuleMaps (cabalTargetId id)
                      (map display $ modulesExtractor target)
                      (defaultExtensions $ buildInfoExtractor target)

collectModuleMaps :: String -> [String] -> [Extension] -> MapBundle
collectModuleMaps targetName modules extensions =
    ( HM.fromList $ map (, targetName) modules
    , one (targetName, map showExt extensions)
    )

showExt :: Extension -> String
showExt (EnableExtension ext)   = show ext
showExt (DisableExtension ext)  = "No" ++ show ext
showExt (UnknownExtension name) = name
