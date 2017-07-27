{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TupleSections #-}

-- | Functions to retrieve and store mapping from modules to their
-- targets and extensions.

module Importify.Cabal.Target
       ( ExtensionsMap
       , TargetsMap
       , MapBundle
       , getMapBundle
       , packageTargets
       ) where

import           Universum                       hiding (fromString)

import           Data.Aeson                      (FromJSON (..), FromJSONKey, ToJSON (..),
                                                  ToJSONKey, Value (String), withText)
import           Data.Hashable                   (Hashable)
import qualified Data.HashMap.Strict             as HM
import qualified Data.Text                       as T (split)
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
import           Importify.Cabal.Package         (extractFromTargets)

type    TargetsMap = HashMap FilePath TargetId
type ExtensionsMap = HashMap TargetId [String]
type MapBundle     = (TargetsMap, ExtensionsMap)

data TargetId = LibraryId
              | ExecutableId Text
              | TestSuiteId  Text
              | BenchmarkId  Text
              deriving (Eq, Generic)

instance Hashable TargetId

instance ToJSON TargetId where
    toJSON = String . cabalTargetId

cabalTargetId :: TargetId -> Text
cabalTargetId LibraryId               = "library"
cabalTargetId (ExecutableId exeName)  = "executable@" <> exeName
cabalTargetId (TestSuiteId testName)  = "test-suite@" <> testName
cabalTargetId (BenchmarkId benchName) = "benchmark@"  <> benchName

instance FromJSON TargetId where
    parseJSON = withText "targetId" $ \targetText -> do
        let targetName = T.split (== '@') targetText
        case targetName of
           ["library"]              -> pure   LibraryId
           ["executable", exeName]  -> pure $ ExecutableId exeName
           ["test-suite", testName] -> pure $ TestSuiteId testName
           ["benchmark", benchName] -> pure $ BenchmarkId benchName
           _ -> fail $ "Unexpected target: " ++ toString targetText

instance   ToJSONKey TargetId
instance FromJSONKey TargetId

packageTargets :: GenericPackageDescription -> [TargetId]
packageTargets = extractFromTargets (const LibraryId)
                                    (ExecutableId . toText . exeName)
                                    (TestSuiteId . toText . testName)
                                    (BenchmarkId . toText . benchmarkName)

getMapBundle :: Path Abs Dir -> GenericPackageDescription -> IO MapBundle
getMapBundle projectPath GenericPackageDescription{..} = do
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
                       -> (Text -> TargetId)
                       -> (TargetId -> target -> IO MapBundle)
                       -> IO ([TargetsMap], [ExtensionsMap])
collectTargetsListMaps treeList idConstructor mapBundler = do
    bundles <- forM treeList $ \(name, condTree) ->
        mapBundler (idConstructor $ toText name) $ condTreeData condTree
    return $ unzip bundles

collectTargetMaps :: (target -> IO [Path Abs File])
                  -> (target -> BuildInfo)
                  -> TargetId
                  -> target
                  -> IO MapBundle
collectTargetMaps modulePathsExtractor buildInfoExtractor id target = do
    pathsToModules <- modulePathsExtractor target
    return $ collectModuleMaps id
                               (map fromAbsFile pathsToModules)
                               (defaultExtensions $ buildInfoExtractor target)

collectModuleMaps :: TargetId -> [FilePath] -> [Extension] -> MapBundle
collectModuleMaps targetId modules extensions =
    ( HM.fromList $ map (, targetId) modules
    , one (targetId, map showExt extensions)
    )
