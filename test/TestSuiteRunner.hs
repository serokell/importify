{-# LANGUAGE TemplateHaskell #-}

module Main where

import           Universum

import           Data.Algorithm.Diff   (Diff (Both), getDiff)
import           Data.List             (sort)
import qualified Data.Text             as T
import           Language.Haskell.Exts (Comment (..), ImportDecl (..),
                                        ModuleHeadAndImports (..), NonGreedy (..),
                                        ParseResult (..), SrcSpanInfo (..),
                                        defaultParseMode, fromParseResult, parse,
                                        parseFileContentsWithComments, prettyPrint)
import           Path                  (Dir, Path, Rel, fromRelDir, fromRelFile, mkRelDir,
                                        parseRelDir, parseRelFile, (</>))
import           System.Directory      (listDirectory)

import           Test.Hspec            (Spec, describe, hspec, it, runIO, shouldBe)

import           Importify.Main        (doAst, doCache)
import Importify.Syntax (debugAST)

main :: IO ()
main = do
--    doCache "importify.cabal"  -- TODO: temporal workaround to make tests work;
--            False              --       to be removed after enhancing test system
--            []
    testFolders <- listDirectory (fromRelDir testDataPath)
    hspec $ mapM_ spec testFolders

testDataPath :: Path Rel Dir
testDataPath = $(mkRelDir "test/test-data/")

spec :: FilePath -> Spec
spec testDir = do
    testDirPath      <- runIO $ parseRelDir testDir
    let testFilesPath = testDataPath </> testDirPath
    testFiles        <- runIO $ sort <$> listDirectory (fromRelDir testFilesPath)

    describe ("folder: " ++ testDir) $ mapM_ (makeTest testFilesPath) testFiles

makeTest :: Path Rel Dir -> FilePath -> Spec
makeTest testDirPath testCaseFile = do
  if testCaseFile `elem` ["07-BigMix.hs"] then do
    diff <- runIO $ loadTestDataDiff testDirPath testCaseFile
    it testCaseFile $ diff `shouldBe` []
  else pure ()

loadTestDataDiff :: Path Rel Dir -> FilePath -> IO [Diff Text]
loadTestDataDiff testDirPath testCaseFile = do
    testCaseFilePath    <- parseRelFile testCaseFile
    let pathToTestCase   = testDirPath </> testCaseFilePath

    testCaseFileContent <- readFile (fromRelFile pathToTestCase)
    putText testCaseFileContent
    let (ast, comments)  = fromParseResult
                         $ parseFileContentsWithComments defaultParseMode
                                                         (toString testCaseFileContent)
    processedSources    <- doAst testCaseFileContent ast
    putText "---------------------------------"
    putText processedSources
    let testSources      = stripComments processedSources
    let extractedSources = getResultSourcesFromComments comments
    debugAST "SRIPPED"  testSources
    debugAST "COMMENTS" extractedSources

    return $ filter isDivergent $ getDiff testSources extractedSources

stripComments :: Text -> [Text]
stripComments = map (T.strip . stripEndLineComment) . lines

stripEndLineComment :: Text -> Text
stripEndLineComment line = case T.breakOnAll "--" line of
    []               -> line
    ((stripped,_):_) -> stripped

getResultSourcesFromComments :: [Comment] -> [Text]
getResultSourcesFromComments = map (T.strip . toText . extractComment)
  where
    extractComment (Comment _ _ comment) = comment

isDivergent :: Diff Text -> Bool
isDivergent (Both _ _) = False
isDivergent _          = True
