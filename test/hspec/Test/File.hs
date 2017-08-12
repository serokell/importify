{-# LANGUAGE TemplateHaskell #-}

-- | Tests for @importify file@ command.

module Test.File
       ( spec
       ) where

import           Universum

import           Data.Algorithm.Diff (Diff (Both), getDiff)
import           Data.List           (sort)
import           Path                (Dir, File, Path, Rel, fileExtension, fromRelDir,
                                      fromRelFile, parseRelDir, parseRelFile, (-<.>),
                                      (</>))
import           System.Directory    (listDirectory)
import           System.Wlog         (Severity)

import           Test.Hspec          (Spec, describe, it, runIO, shouldBe, xit)

import           Importify.Main      (importifyFileContent)
import           Importify.Path      (testDataPath)

spec :: Spec
spec = do
    testFolders <- runIO $ listDirectory (fromRelDir testDataPath)
    describe "file:unused" $ mapM_ makeTestGroup testFolders

makeTestGroup :: FilePath -> Spec
makeTestGroup testDir = do
    testDirPath      <- runIO $ parseRelDir testDir
    let testCasesPath = testDataPath </> testDirPath
    testDirPaths     <- runIO $ mapM parseRelFile =<<
                                listDirectory (fromRelDir testCasesPath)
    let testHsOnly = sort $ filter ((== ".hs") . fileExtension) testDirPaths
    describe ("subfolder: " ++ testDir) $ mapM_ (makeTest testCasesPath) testHsOnly

makeTest :: Path Rel Dir -> Path Rel File -> Spec
makeTest testDirPath testCasePath = do
    diff <- runIO $ loadTestDataDiff testDirPath testCasePath
    let filename = fromRelFile testCasePath
    (if elem filename pendingTests then xit else it) filename $ diff `shouldBe` []

pendingTests :: [String]
pendingTests = ["01-ImportBothUsedQualified.hs" -- Importify can't modify source yet
               ]

loadTestDataDiff :: Path Rel Dir -> Path Rel File -> IO [Diff Text]
loadTestDataDiff testDirPath testCasePath = do
    let fullPathToTest = testDirPath </> testCasePath
    goldenExamplePath <- fullPathToTest -<.> ".golden"

    goldenExampleSrc     <- readFile (fromRelFile goldenExamplePath)
    Right importifiedSrc <- importifyFileContent fullPathToTest

    return $ filter isDivergent $ getDiff (lines importifiedSrc)
                                          (lines goldenExampleSrc)

isDivergent :: Diff Text -> Bool
isDivergent (Both _ _) = False
isDivergent _          = True
