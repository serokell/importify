{-# LANGUAGE TupleSections #-}

-- | Functions to retrieve and store mapping from modules to their
-- targets and extensions.

module Importify.Cabal.Target
       ( ExtensionsMap
       , TargetsMap
       , MapBundle
       , getExtensionMaps
       ) where

import           Universum                       hiding (fromString)

import qualified Data.HashMap.Strict             as HM
import           Distribution.ModuleName         (ModuleName)
import           Distribution.PackageDescription (Benchmark (..), BenchmarkInterface (..),
                                                  BuildInfo (..), CondTree,
                                                  Executable (..),
                                                  GenericPackageDescription (..),
                                                  Library (..), TestSuite (..),
                                                  TestSuiteInterface (..), condTreeData)
import           Language.Haskell.Extension      (Extension (..))
import           Path                            (Abs, Dir, File, Path, fromAbsFile)

import           Importify.Cabal.Extension       (showExt)
import           Importify.Cabal.Module          (modulePaths)


type    TargetsMap = HashMap FilePath String
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

getExtensionMaps :: Path Abs Dir -> GenericPackageDescription -> IO MapBundle
getExtensionMaps projectPath GenericPackageDescription{..} = do
    (libTM,    libEM)    <- libMaps
    (exeTMs,   exeEMs)   <- exeMaps
    (testTMs,  testEMs)  <- testMaps
    (benchTMs, benchEMs) <- benchMaps

    return ( HM.unions $ libTM : exeTMs ++ testTMs ++ benchTMs
           , HM.unions $ libEM : exeEMs ++ testEMs ++ benchEMs
           )
  where
    projectPaths :: BuildInfo -> Either [ModuleName] FilePath -> IO [Path Abs File]
    projectPaths = modulePaths projectPath

    libPaths :: Library -> IO [Path Abs File]
    libPaths Library{..} = projectPaths libBuildInfo (Left exposedModules)

    exePaths :: Executable -> IO [Path Abs File]
    exePaths Executable{..} = projectPaths buildInfo (Right modulePath)

    testPaths :: TestSuite -> IO [Path Abs File]
    testPaths TestSuite{..} = projectPaths testBuildInfo $ case testInterface of
        TestSuiteExeV10 _ path -> Right path
        TestSuiteLibV09 _ name -> Left [name]
        TestSuiteUnsupported _ -> Left []

    benchPaths :: Benchmark -> IO [Path Abs File]
    benchPaths Benchmark{..} = projectPaths benchmarkBuildInfo $ case benchmarkInterface of
        BenchmarkExeV10 _ path -> Right path
        BenchmarkUnsupported _ -> Left []

    libMaps :: IO MapBundle
    libMaps = maybe mempty
                    ( collectTargetMaps libPaths
                                        libBuildInfo
                                        LibraryId
                    . condTreeData)
                    condLibrary
    exeMaps :: IO ([TargetsMap], [ExtensionsMap])
    exeMaps = collectTargetsListMaps condExecutables
                                     ExecutableId
                                     (collectTargetMaps exePaths buildInfo)

    testMaps :: IO ([TargetsMap], [ExtensionsMap])
    testMaps = collectTargetsListMaps condTestSuites
                                      TestSuiteId
                                      (collectTargetMaps testPaths testBuildInfo)

    benchMaps :: IO ([TargetsMap], [ExtensionsMap])
    benchMaps = collectTargetsListMaps condBenchmarks
                                       BenchmarkId
                                       (collectTargetMaps benchPaths benchmarkBuildInfo)


-- | Generalized 'MapBundle' collector for executables, testsuites and
-- benchmakrs parts of package.
collectTargetsListMaps :: [(String, CondTree v c target)]
                       -> (String -> TargetId)
                       -> (TargetId -> target -> IO MapBundle)
                       -> IO ([TargetsMap], [ExtensionsMap])
collectTargetsListMaps treeList idConstructor mapBundler = do
    bundles <- forM treeList $ \(name, condTree) ->
        mapBundler (idConstructor name) $ condTreeData condTree
    return $ unzip bundles

collectTargetMaps :: (target -> IO [Path Abs File])
                  -> (target -> BuildInfo)
                  -> TargetId
                  -> target
                  -> IO MapBundle
collectTargetMaps modulePathsExtractor buildInfoExtractor id target = do
    pathsToModules <- modulePathsExtractor target
    return $ collectModuleMaps (cabalTargetId id)
                               (map fromAbsFile pathsToModules)
                               (defaultExtensions $ buildInfoExtractor target)

collectModuleMaps :: String -> [FilePath] -> [Extension] -> MapBundle
collectModuleMaps targetName modules extensions =
    ( HM.fromList $ map (, targetName) modules
    , one (targetName, map showExt extensions)
    )
