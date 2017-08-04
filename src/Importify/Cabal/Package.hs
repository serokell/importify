-- | Utility functions to work with 'GenericPackageDescription' and
-- other miscellaneous stuff in .cabal files.

module Importify.Cabal.Package
       ( extractFromTargets
       , packageDependencies
       , readCabal
       ) where

import           Universum                             hiding (fromString)

import           Distribution.Package                  (Dependency (..), PackageName (..))
import           Distribution.PackageDescription       (Benchmark (benchmarkBuildInfo),
                                                        BuildInfo (..), CondTree,
                                                        Executable (..),
                                                        GenericPackageDescription (..),
                                                        Library (..),
                                                        TestSuite (testBuildInfo),
                                                        condTreeData)
import           Distribution.PackageDescription.Parse (readPackageDescription)
import           Distribution.Verbosity                (normal)

readCabal :: FilePath -> IO GenericPackageDescription
readCabal = readPackageDescription normal

dependencyName :: Dependency -> String
dependencyName (Dependency PackageName{..} _) = unPackageName

-- | Retrieve list of unique names for all package dependencies inside
-- library, all executables, all test suites and all benchmarks for a
-- given package.
packageDependencies :: GenericPackageDescription -> [String]
packageDependencies = ordNub
                    . concatMap (map dependencyName . targetBuildDepends)
                    . allBuildInfos

allBuildInfos :: GenericPackageDescription -> [BuildInfo]
allBuildInfos = extractFromTargets       libBuildInfo
                                            buildInfo
                                        testBuildInfo
                                   benchmarkBuildInfo

-- | Extract some uniform data from every target if it's present.
extractFromTargets :: (Library    -> r)  -- ^ 'Library' extractor
                   -> (Executable -> r)  -- ^ 'Executable' extractor
                   -> (TestSuite  -> r)  -- ^ 'TestSuite' extractor
                   -> (Benchmark  -> r)  -- ^ 'Benchmakr' extractor
                   -> GenericPackageDescription -- ^ Package
                   -> [r]  -- ^ List of results collected from each target
extractFromTargets fromLib fromExe fromTst fromBnc GenericPackageDescription{..} =
  concat
    [ maybe [] (one . fromLib . condTreeData) condLibrary
    , mapTargets fromExe condExecutables
    , mapTargets fromTst condTestSuites
    , mapTargets fromBnc condBenchmarks
    ]

mapTargets :: (t -> r) -> [(String, CondTree v c t)] -> [r]
mapTargets extractor = map (extractor . condTreeData . snd)
