{-# LANGUAGE TemplateHaskell #-}

-- | Tests for @importify file@ command.

module Test.File
       ( spec
       ) where

import           Universum

import           Data.List      (sort)
import           Path           (Abs, Dir, File, Path, Rel, dirname, fileExtension,
                                 filename, fromAbsFile, fromRelDir, fromRelFile,
                                 mkRelFile, (-<.>), (</>))
import           Path.IO        (listDir)
import           System.Wlog    (Severity)

import           Test.Hspec     (Spec, describe, it, runIO, shouldBe, xit)

import           Importify.Main (importifyFileContent)
import           Importify.Path (testUnusedPath)

spec :: Spec
spec = do
    (testFolders, _) <- runIO $ listDir testUnusedPath
    describe "file:unused" $
        mapM_ (makeTestGroup . (testUnusedPath </> ) . dirname) testFolders


makeTestGroup :: Path Rel Dir -> Spec
makeTestGroup testCasesPath = do
    (_, testDirPaths) <- runIO $ listDir testCasesPath
    let testHsOnly     = sort
                       $ filter ((== ".hs") . fileExtension) testDirPaths

    describe ("subfolder: " ++ fromRelDir (dirname testCasesPath)) $
        mapM_ makeTest testHsOnly

makeTest :: Path Abs File -> Spec
makeTest testCasePath = do
    (result, expected) <- runIO $ loadTestData testCasePath
    let testType = if filename testCasePath `elem` pendingTests then xit else it
    testType (fromRelFile $ filename testCasePath) $ result `shouldBe` expected

pendingTests :: [Path Rel File]
pendingTests = [ $(mkRelFile "01-ImportBothUsedQualified.hs") -- Importify can't modify source yet
               ]

loadTestData :: Path Abs File -> IO (Text, Text)
loadTestData testCasePath = do
    goldenExamplePath <- testCasePath -<.> ".golden"

    goldenExampleSrc     <- readFile (fromAbsFile goldenExamplePath)
    Right importifiedSrc <- importifyFileContent testCasePath

    return (importifiedSrc, goldenExampleSrc)
