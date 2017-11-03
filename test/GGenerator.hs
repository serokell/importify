{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

-- | This executable generated .golden tests for importify.

module Main where

import           Universum

import           Path           (Abs, Dir, File, Path, fileExtension, fromAbsFile, (-<.>))
import           Path.IO        (listDirRecur, removeFile)

import           Importify.Main (importifyRemoveWithPath)
import           Importify.Path (testDataPath)

main :: IO ()
main = do
    arguments <- getArgs
    case arguments of
        ["--clean"] -> cleanGoldenExamples
        ["--force"] -> generateGoldenTests
        []          -> generateGoldenTestsPrompt
        _           -> putText "Incorrect arguments!"

findByExtension :: MonadIO m => String -> Path b Dir -> m [Path Abs File]
findByExtension ext path = filter ((== ext) . fileExtension) . snd
                          <$> listDirRecur path

findGoldenFiles :: MonadIO m => Path b Dir -> m [Path Abs File]
findGoldenFiles = findByExtension "golden"

cleanGoldenExamples :: MonadIO m => m ()
cleanGoldenExamples = do
    goldenExamples <- findGoldenFiles testDataPath
    mapM_ removeFile goldenExamples

findHaskellFiles :: MonadIO m => Path b Dir -> m [Path Abs File]
findHaskellFiles = findByExtension ".hs"

writeBinaryFile :: MonadIO m => Path Abs File -> Text -> m ()
writeBinaryFile = writeFile . fromAbsFile

generateGoldenTestsPrompt :: IO ()
generateGoldenTestsPrompt = do
    putText "> Are you sure want to generate new golden examples? [y/N]"
    getLine >>= \case
        "y" -> generateGoldenTests
        _   -> putText "Aborting generation"

generateGoldenTests :: IO ()
generateGoldenTests = do
    testCaseFiles <- findHaskellFiles testDataPath
    forM_ testCaseFiles $ \testCasePath -> do
       Right modifiedSrc <- importifyRemoveWithPath testCasePath
       goldenPath        <- testCasePath -<.> "golden"
       writeBinaryFile goldenPath modifiedSrc
