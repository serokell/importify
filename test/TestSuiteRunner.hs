{-# LANGUAGE TemplateHaskell #-}

module Main where

import           Universum

import           Data.Algorithm.Diff  (Diff (Both), getDiff)
import           Data.List            (partition, sort)
import           Path                 (Dir, File, Path, Rel, fileExtension, fromRelDir,
                                       fromRelFile, parseRelDir, parseRelFile, (-<.>),
                                       (</>))
import           System.Directory     (listDirectory)
import           System.Environment   (withArgs)
import           System.Wlog          (Severity (Info))

import           Test.Hspec           (Spec, describe, hspec, it, runIO, shouldBe, xit)

import           Extended.System.Wlog (initImportifyLogger)
import           Importify.Main       (doCache, doSource)
import           Importify.Paths      (testDataPath)

main :: IO ()
main = do
    (cacheArgs, hspecArgs) <- splitCmdOptions <$> getArgs
    initImportifyLogger Info
    when (null cacheArgs) $ doCache False []

    testFolders <- listDirectory (fromRelDir testDataPath)
    withArgs hspecArgs $ hspec $ mapM_ makeTestGroup testFolders

splitCmdOptions :: [String] -> ([String], [String])
splitCmdOptions = partition (== "--no-cache")

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

    testCasePathContent <- readFile (fromRelFile fullPathToTest)
    importifiedSrc      <- doSource (fromRelFile testCasePath) testCasePathContent
    goldenExampleSrc    <- readFile (fromRelFile goldenExamplePath)

    return $ filter isDivergent $ getDiff (lines importifiedSrc)
                                          (lines goldenExampleSrc)

isDivergent :: Diff Text -> Bool
isDivergent (Both _ _) = False
isDivergent _          = True
