{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

-- | This executable generated .golden tests for importify.

module Main where

import           Universum  hiding (writeFile)

import           Data.Text.IO      (writeFile)

import           Path              (Abs, Dir, File, Path,
                                    fileExtension, fromAbsFile,
                                    (-<.>))
import           Path.IO           (listDirRecur, removeFile)

import           Importify.Main    (importifyFileContent)
import           Importify.Path    (testDataPath)

main :: IO ()
main = do
    arguments <- getArgs
    case arguments of
        ["--clean"] -> cleanGoldenExamples
        ["--force"] -> generateGoldenTestsPrompt True
        []          -> generateGoldenTestsPrompt False
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


generateGoldenTestsPrompt :: Bool -> IO ()
generateGoldenTestsPrompt True  = generateGoldenTests
generateGoldenTestsPrompt False = do
    putText "> Are you sure want to generate new golden examples? [y/N]"
    getLine >>= \case
        "y" -> generateGoldenTests
        _   -> putText "Aborting generation"

findHaskellFiles :: MonadIO m => Path b Dir -> m [Path Abs File]
findHaskellFiles = findByExtension "hs"

writeBinaryFile :: Path Abs File -> Text -> IO ()
writeBinaryFile = writeFile . fromAbsFile

generateGoldenTests :: IO ()
generateGoldenTests = do
    testCaseFiles <- findHaskellFiles testDataPath
    forM_ testCaseFiles $ \testCasePath -> do
       Right modifiedSrc <- importifyFileContent testCasePath
       goldenPath        <- testCasePath -<.> "golden"
       writeBinaryFile goldenPath modifiedSrc
