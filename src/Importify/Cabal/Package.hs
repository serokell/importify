-- | Utility functions to work with 'GenericPackageDescription' and
-- other miscellaneous stuff in .cabal files.

module Importify.Cabal.Package
       ( packageDependencies
       , readCabal
       , withLibrary
       ) where

import           Universum                             hiding (fromString)

import qualified Data.HashMap.Strict                   as Map
import           Data.List                             (partition)
import           Distribution.ModuleName               (ModuleName, fromString,
                                                        toFilePath)
import qualified Distribution.ModuleName               as Cabal
import           Distribution.Package                  (Dependency (..), PackageName (..))
import           Distribution.PackageDescription       (Benchmark (benchmarkBuildInfo),
                                                        BuildInfo (..), CondTree,
                                                        Executable (..),
                                                        GenericPackageDescription (..),
                                                        Library (..),
                                                        TestSuite (testBuildInfo),
                                                        condTreeData, exeModules,
                                                        libModules)
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

readCabal :: FilePath -> IO GenericPackageDescription
readCabal = readPackageDescription normal

-- | Perform given action with package library 'BuilInfo'
-- if 'Library' is present. We care only about library exposed modules
-- because only they can be imported outside that package.
withLibrary :: (Applicative f, Monoid m)
            => GenericPackageDescription
            -> (Library -> [HSE.Extension] -> f m)
            -> f m
withLibrary GenericPackageDescription{..} action =
    maybe (pure mempty)
          (\treeNode -> let library       = condTreeData treeNode
                            BuildInfo{..} = libBuildInfo library
                            extensions    = filter isHseExt $ defaultExtensions ++ otherExtensions
                        in action library (map cabalExtToHseExt extensions)
          )
          condLibrary

cabalExtToHseExt :: Extension -> HSE.Extension
cabalExtToHseExt = {- trace ("Arg = " ++ show ext ++ "") -} read . show

isHseExt :: Extension -> Bool
isHseExt (EnableExtension NegativeLiterals) = False
isHseExt (EnableExtension Unsafe)           = False
isHseExt _                                  = True

dependencyName :: Dependency -> String
dependencyName (Dependency PackageName{..} _) = unPackageName

-- | Retrieve list of unique names for all package dependencies inside
-- library, all executables, all test suites and all benchmarks for a
-- given package.
--
-- TODO: what about version bounds?
packageDependencies :: GenericPackageDescription -> [String]
packageDependencies = ordNub
                    . concatMap (map dependencyName . targetBuildDepends)
                    . allBuildInfos

allBuildInfos :: GenericPackageDescription -> [BuildInfo]
allBuildInfos GenericPackageDescription{..} = concat
    [ maybe [] (one . libBuildInfo . condTreeData) condLibrary
    , collectBuildInfos          buildInfo condExecutables
    , collectBuildInfos      testBuildInfo condTestSuites
    , collectBuildInfos benchmarkBuildInfo condBenchmarks
    ]

collectBuildInfos :: (t -> BuildInfo) -> [(String, CondTree v c t)] -> [BuildInfo]
collectBuildInfos extractor = map (extractor . condTreeData . snd)

-- This function works but isn't used anywhere
{-
findModuleBuildInfo :: String -> GenericPackageDescription -> Maybe BuildInfo
findModuleBuildInfo modNameStr pkg@GenericPackageDescription{..} =
    lookupExecutable <|> lookupExposedModules <|> lookupOtherModules
  where
    modName = fromString modNameStr
    lookupExecutable = asum $ do
        (_name, condExecutable) <- condExecutables
        let exec = condTreeData condExecutable
        if (modulePath exec) == (toFilePath modName ++ ".hs") then
            pure $ Just (buildInfo exec)
        else
            pure Nothing
    lookupExposedModules = case condLibrary of
        Just condTree ->
            if elem modName (exposedModules lib) then Just (libBuildInfo lib) else Nothing
            where lib = condTreeData condTree
        Nothing -> Nothing
    lookupOtherModules = find (elem modName . otherModules) $ getBuildInfos pkg
-}
