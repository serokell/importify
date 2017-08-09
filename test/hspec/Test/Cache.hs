-- | Tests for @importify cache@ command.

module Test.Cache
       ( modulesMapSpec
       ) where

import           Universum

import qualified Data.HashMap.Strict as HM
import           Lens.Micro.Platform (at, (?=))
import           System.Directory    (getCurrentDirectory)
import           System.FilePath     ((</>))

import           Test.Hspec          (Spec, describe, it, runIO, shouldSatisfy)

import           Importify.Cabal     (ModulesBundle (..), ModulesMap, TargetId (..))
import           Importify.Path      (decodeFileOrMempty)

-- | 'Spec' for modules mapping.
modulesMapSpec :: Spec
modulesMapSpec = do
    testMap  <- runIO createTestModulesMap
    cacheMap <- runIO $ decodeFileOrMempty ".importify/modules" return
    describe "cache:targets map" $
        it "Test map is subset of cache" $
            testMap `shouldSatisfy` (`subsetOf` cacheMap)

createTestModulesMap :: IO ModulesMap
createTestModulesMap = do
    curDir <- getCurrentDirectory
    let withDir = (curDir </>)
    return $ executingState mempty $ do
        at (withDir "src/Importify/Path.hs") ?=  -- Simple file from library
            ModulesBundle "importify-1.0" "Importify.Path" LibraryId
        at (withDir "src/Importify/Main.hs") ?=  -- autoexported file
            ModulesBundle "importify-1.0" "Importify.Main" LibraryId
        at (withDir "src/Extended/Data/List.hs") ?=  -- file from other-modules
            ModulesBundle "importify-1.0" "Extended.Data.List" LibraryId
        at (withDir "app/Main.hs") ?=  -- Main file from executable
            ModulesBundle "importify-1.0" "Main" (ExecutableId "importify")
        at (withDir "app/Options.hs") ?=  -- other file from executable
            ModulesBundle "importify-1.0" "Options" (ExecutableId "importify")
        at (withDir "test/hspec/Runner.hs") ?=  -- Main file for tests
            ModulesBundle "importify-1.0" "Main" (TestSuiteId "importify-test")
        at (withDir "test/hspec/Test/File.hs") ?=  -- Other file from tests
            ModulesBundle "importify-1.0" "Test.File" (TestSuiteId "importify-test")


-- | @testMap `subsetOf` cacheMap@ returns 'True' iff all entries from
-- @testMap@ are in @biggerMap@.
subsetOf :: ModulesMap -> ModulesMap -> Bool
testMap `subsetOf` cacheMap = all isInCacheMap (HM.toList testMap)
  where
    isInCacheMap :: (FilePath, ModulesBundle) -> Bool
    isInCacheMap (path, bundle) = maybe False (== bundle)
                                $ HM.lookup path cacheMap
