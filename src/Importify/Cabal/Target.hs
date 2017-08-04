{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TupleSections #-}

-- | Functions to retrieve and store mapping from modules to their
-- targets and extensions.

module Importify.Cabal.Target
       ( -- * Maps from modules paths to cache parts
         ExtensionsMap
       , ModulesMap

         -- * Target types
       , ModulesBundle (..)
       , TargetId      (..)

         -- * Utilities to extract targets
       , extractTargetBuildInfo
       , extractTargetsMap
       , packageExtensions
       , packageTargets
       , targetIdDir
       ) where

import           Universum                       hiding (fromString)

import           Data.Aeson                      (FromJSON (parseJSON),
                                                  FromJSONKey (fromJSONKey),
                                                  FromJSONKeyFunction (FromJSONKeyTextParser),
                                                  ToJSON (toJSON), ToJSONKey (toJSONKey),
                                                  Value (String), object, withObject,
                                                  withText, (.:), (.=))
import           Data.Aeson.Types                (Parser, toJSONKeyText)
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
import           Language.Haskell.Exts           (prettyExtension)
import           Path                            (Abs, Dir, File, Path, fromAbsFile)

import           Importify.Cabal.Extension       (buildInfoExtensions)
import           Importify.Cabal.Module          (modulePaths)

-- | Mapping from module path to its package and module name.
type    ModulesMap = HashMap FilePath ModulesBundle  -- cached globally
type    TargetsMap = HashMap FilePath TargetId       -- not cached
type ExtensionsMap = HashMap TargetId [String]       -- cached per project package

data TargetId = LibraryId
              | ExecutableId !Text
              | TestSuiteId  !Text
              | BenchmarkId  !Text
              deriving (Show, Eq, Generic)

instance Hashable TargetId

instance ToJSON TargetId where
    toJSON = String . targetIdDir

instance ToJSONKey TargetId where
    toJSONKey = toJSONKeyText targetIdDir

-- | Directory name for corresponding target.
targetIdDir :: TargetId -> Text
targetIdDir LibraryId               = "library"
targetIdDir (ExecutableId exeName)  = "executable@" <> exeName
targetIdDir (TestSuiteId testName)  = "test-suite@" <> testName
targetIdDir (BenchmarkId benchName) = "benchmark@"  <> benchName

instance FromJSON TargetId where
    parseJSON = withText "targetId" targetIdParser

instance FromJSONKey TargetId where
    fromJSONKey = FromJSONKeyTextParser targetIdParser

targetIdParser :: Text -> Parser TargetId
targetIdParser targetText = do
    let targetName = T.split (== '@') targetText
    case targetName of
        ["library"]              -> pure   LibraryId
        ["executable", exeName]  -> pure $ ExecutableId exeName
        ["test-suite", testName] -> pure $ TestSuiteId testName
        ["benchmark", benchName] -> pure $ BenchmarkId benchName
        _                        -> fail $ "Unexpected target: " ++ toString targetText

-- | All data for given module. This is needed to locate all required
-- information about module by its path.
data ModulesBundle = ModulesBundle
    { mbPackage :: !Text      -- ^ Module package, like @importify-1.0@
    , mbModule  :: !String    -- ^ Full module name, like @Importify.Main@
    , mbTarget  :: !TargetId  -- ^ Target of module
    } deriving (Show)

instance ToJSON ModulesBundle where
    toJSON ModulesBundle{..} = object
        [ "package" .= mbPackage
        , "module"  .= mbModule
        , "target"  .= mbTarget
        ]

instance FromJSON ModulesBundle where
    parseJSON = withObject "ModulesBundle" $ \obj -> do
        mbPackage <- obj .: "package"
        mbModule  <- obj .: "module"
        mbTarget  <- obj .: "target"
        pure ModulesBundle{..}

-- | Extract every 'TargetId' for given project description.
packageTargets :: GenericPackageDescription -> [TargetId]
packageTargets GenericPackageDescription{..} =
  concat
    [ maybe [] (\_ -> [LibraryId]) condLibrary
    , targetMap ExecutableId condExecutables
    , targetMap TestSuiteId  condTestSuites
    , targetMap BenchmarkId  condBenchmarks
    ]
  where
    targetMap tid = map (tid . toText . fst)

-- | Extracts 'BuildInfo' for given 'TargetId'.
extractTargetBuildInfo
    :: TargetId
    -> GenericPackageDescription
    -> Maybe BuildInfo
extractTargetBuildInfo LibraryId = fmap (libBuildInfo . condTreeData) . condLibrary
extractTargetBuildInfo (ExecutableId name) =
    findTargetBuildInfo buildInfo name . condExecutables
extractTargetBuildInfo (TestSuiteId name) =
    findTargetBuildInfo testBuildInfo name . condTestSuites
extractTargetBuildInfo (BenchmarkId name) =
    findTargetBuildInfo benchmarkBuildInfo name . condBenchmarks

findTargetBuildInfo :: (target -> info)
                    -> Text
                    -> [(String, CondTree v c target)]
                    -> Maybe info
findTargetBuildInfo toInfo name = fmap (toInfo . condTreeData . snd)
                                . find ((== name) . toText . fst)

-- | Extracts mapping from each package target to its extensions enabled by default.
packageExtensions :: [TargetId] -> GenericPackageDescription -> ExtensionsMap
packageExtensions targetIds desc = mconcat $ mapMaybe targetToExtensions targetIds
  where
    targetToExtensions :: TargetId -> Maybe ExtensionsMap
    targetToExtensions targetId = toMap targetId <$> extractTargetBuildInfo targetId desc

    toMap :: TargetId -> BuildInfo -> ExtensionsMap
    toMap targetId info = one (targetId, map prettyExtension $ buildInfoExtensions info)

-- | This function extracts 'ModulesMap' from given package by given
-- full path to project root directory.
extractTargetsMap :: Path Abs Dir -> GenericPackageDescription -> IO TargetsMap
extractTargetsMap projectPath GenericPackageDescription{..} = do
    libTM    <- libMap
    exeTMs   <- exeMaps
    testTMs  <- testMaps
    benchTMs <- benchMaps

    return $ HM.unions $ libTM : exeTMs ++ testTMs ++ benchTMs
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

    libMap :: IO TargetsMap
    libMap = maybe mempty
                   ( collectTargetsMap libPaths
                                       LibraryId
                   . condTreeData)
                   condLibrary

    exeMaps :: IO [TargetsMap]
    exeMaps = collectTargetsListMaps condExecutables
                                     ExecutableId
                                     (collectTargetsMap exePaths)

    testMaps :: IO [TargetsMap]
    testMaps = collectTargetsListMaps condTestSuites
                                      TestSuiteId
                                      (collectTargetsMap testPaths)

    benchMaps :: IO [TargetsMap]
    benchMaps = collectTargetsListMaps condBenchmarks
                                       BenchmarkId
                                       (collectTargetsMap benchPaths)


-- | Generalized 'TargetsMap' collector for executables, testsuites and
-- benchmakrs of package.
collectTargetsListMaps :: [(String, CondTree v c target)]
                       -> (Text -> TargetId)
                       -> (TargetId -> target -> IO TargetsMap)
                       -> IO [TargetsMap]
collectTargetsListMaps treeList idConstructor mapBundler =
    forM treeList $ \(name, condTree) ->
        mapBundler (idConstructor $ toText name) $ condTreeData condTree

collectTargetsMap :: (target -> IO [Path Abs File])
                  -> TargetId
                  -> target
                  -> IO TargetsMap
collectTargetsMap modulePathsExtractor targetId target = do
    pathsToModules <- modulePathsExtractor target
    return $ constructModulesMap (map fromAbsFile pathsToModules)
  where
    constructModulesMap :: [FilePath] -> TargetsMap
    constructModulesMap = HM.fromList . map (, targetId)
