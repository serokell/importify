module Main where

import           Universum

import           Data.List             (partition)
import           System.Environment    (withArgs)
import           System.Wlog           (Severity (Info))

import           Test.Hspec            (hspec)

import           Extended.System.Wlog  (initImportifyLogger)
import           Importify.Environment (runCache)
import           Importify.Main        (importifyCacheProject)

import qualified Test.Cache
import qualified Test.File

main :: IO ()
main = do
    (cacheArgs, hspecArgs) <- splitCmdOptions <$> getArgs
    initImportifyLogger Info
    when (null cacheArgs) $ runCache False importifyCacheProject

    withArgs hspecArgs $ hspec $ do
       Test.Cache.modulesMapSpec
       Test.File.spec

splitCmdOptions :: [String] -> ([String], [String])
splitCmdOptions = partition (== "--no-cache")
