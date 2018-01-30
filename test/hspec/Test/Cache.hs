{-# LANGUAGE TemplateHaskell #-}

-- | Tests for @importify cache@ command.

module Test.Cache
       ( modulesMapSpec
       ) where

import Universum

import Data.Version (showVersion)
import Lens.Micro.Platform (at, (?=))
import Path (fromAbsFile, mkRelFile, (</>))
import Path.IO (getCurrentDir)

import Test.Hspec (Spec, describe, it, runIO, shouldSatisfy)

import Importify.Cabal (ModulesBundle (..), ModulesMap, TargetId (..), readCabal)
import Importify.Path (decodeFileOrMempty, importifyPath, modulesPath)
import Paths_importify (version)

import qualified Data.HashMap.Strict as HM

getVersion :: Text
getVersion = toText $ showVersion version

-- | 'Spec' for modules mapping.
modulesMapSpec :: Spec
modulesMapSpec = do
    testMap  <- runIO createTestModulesMap
    cacheMap <- runIO $ decodeFileOrMempty (importifyPath </> modulesPath) return
    describe "cache:targets map" $
        it "Test map is subset of cache" $
            testMap `shouldSatisfy` (`subsetOf` cacheMap)

createTestModulesMap :: IO ModulesMap
createTestModulesMap = do
    curDir <- getCurrentDir
    let withDir path = fromAbsFile $ curDir </> path
    let importify = "importify-" <> getVersion
    return $ executingState mempty $ do
        at (withDir $(mkRelFile "src/Importify/Path.hs")) ?=  -- Simple file from library
            ModulesBundle importify "Importify.Path" LibraryId
        at (withDir $(mkRelFile "src/Importify/Main.hs")) ?=  -- autoexported file
            ModulesBundle importify "Importify.Main" LibraryId
        at (withDir $(mkRelFile "src/Extended/Data/List.hs")) ?=  -- file from other-modules
            ModulesBundle importify "Extended.Data.List" LibraryId
        at (withDir $(mkRelFile "app/Main.hs")) ?=  -- Main file from executable
            ModulesBundle importify "Main" (ExecutableId "importify")
        at (withDir $(mkRelFile "app/Options.hs")) ?=  -- other file from executable
            ModulesBundle importify "Options" (ExecutableId "importify")
        at (withDir $(mkRelFile "test/hspec/Runner.hs")) ?=  -- Main file for tests
            ModulesBundle importify "Main" (TestSuiteId "importify-test")
        at (withDir $(mkRelFile "test/hspec/Test/File.hs")) ?=  -- Other file from tests
            ModulesBundle importify "Test.File" (TestSuiteId "importify-test")


-- | @testMap `subsetOf` cacheMap@ returns 'True' iff all entries from
-- @testMap@ are in @biggerMap@.
subsetOf :: ModulesMap -> ModulesMap -> Bool
testMap `subsetOf` cacheMap = all isInCacheMap (HM.toList testMap)
  where
    isInCacheMap :: (FilePath, ModulesBundle) -> Bool
    isInCacheMap (path, bundle) = (== Just bundle) $ HM.lookup path cacheMap
