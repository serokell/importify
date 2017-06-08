{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module SystemSuite where

import           Universum

import           Control.Exception.Base (throw)
import           Data.List              (isPrefixOf, isSuffixOf, sort)
import qualified Data.Text              as T
import           Distribution.TestSuite (Progress (..), Result (..), Test (..),
                                         TestInstance (..))
import           Importify.Common       (Identifier (..), parseForImports)
import           Language.Haskell.Exts  (ImportDecl (..), ModuleHeadAndImports (..),
                                         NonGreedy (..), ParseResult (..),
                                         SrcSpanInfo (..), fromParseResult, parse,
                                         prettyPrint)
import           System.Directory       (doesFileExist, getCurrentDirectory,
                                         listDirectory)
import           System.Exit            (ExitCode (..))
import           Turtle                 (ProcFailed (..), empty, procStrictWithErr)

tests :: IO ([Test])
tests = do
    importifyExists <- checkImportifyExists
    cwd <- getCurrentDirectory
    case importifyExists of
        True -> do
            testFiles <- filter (\file ->
                                     (isPrefixOf "Test" file) &&
                                     (isSuffixOf ".hs" file))
                            <$> (listDirectory $ toString testDirectory)
            putText ""
            putStrLn $ "Running " ++ (show $ length testFiles) ++ " tests"
            mapM (makeTest . (toString testDirectory ++)) $ sort testFiles
        False ->
            pure $ (:[]) $ Test $ TestInstance
                { run = pure $ Finished $ Fail $
                    "Importify binary not found in " ++ cwd
                , name = "importifyBinary"
                , tags = []
                , options = []
                , setOption = \_ _ -> Left "Options not supported"
                }

makeTest :: FilePath -> IO (Test)
makeTest file = do
    (unusedSymbols, usedImports) <- loadTestData file
    return $ Test $ TestInstance { run = executeTest file unusedSymbols usedImports
                                 , name = file
                                 , tags = []
                                 , options = []
                                 , setOption = \_ _ -> Left "Options not supported"
                                 }

loadTestData :: FilePath -> IO ([Identifier], [ImportDecl SrcSpanInfo])
loadTestData file = do
    fileContents <- readFile file
    let unused:imports = takeWhile (T.isPrefixOf "-- ") $ lines fileContents
    return (parseUnused $ toText unused, parseImports $ map toText imports)

parseUnused :: Text -> [Identifier]
parseUnused = map (Identifier . toString) . filter (/= "") . map T.strip . T.splitOn "," . uncomment

parseImports :: [Text] -> [ImportDecl SrcSpanInfo]
parseImports imports =
    let src = unlines $ map uncomment imports
        parseResult :: ParseResult (NonGreedy (ModuleHeadAndImports SrcSpanInfo))
        parseResult = parse $ toString src
        NonGreedy (ModuleHeadAndImports _ _pragma _head importDecls) =
            fromParseResult parseResult
    in importDecls

uncomment :: Text -> Text
uncomment = T.drop 3

executeTest :: FilePath -> [Identifier] -> [ImportDecl SrcSpanInfo] -> IO Progress
executeTest file expectUnusedIds expectUsedImports = do
    (importifiedFile, unusedIds) <- importifyFile file
    let (_, usedImports) = parseForImports [] importifiedFile
    unusedSymbolsResult <- checkUnusedSymbols expectUnusedIds unusedIds
    case unusedSymbolsResult of
        Finished Pass -> checkUsedImports expectUsedImports usedImports
        _             -> pure unusedSymbolsResult

checkUnusedSymbols :: [Identifier] -> [Identifier] -> IO Progress
checkUnusedSymbols expectUnusedSymbols actualUnusedSymbols =
    if sort expectUnusedSymbols /= sort actualUnusedSymbols then do
        putStr $ unlines
             [ ""
             , unwords $
               "Expected unused symbols: ":(map (toText . getIdentifier) expectUnusedSymbols)
             , unwords $
               "Actual unused symbols: ":(map (toText . getIdentifier) actualUnusedSymbols)
             ]
        pure $ Finished $ Fail "Invalid unused symbols"
    else
        pure $ Finished $ Pass

checkUsedImports :: [ImportDecl SrcSpanInfo] -> [ImportDecl SrcSpanInfo] -> IO Progress
checkUsedImports expectUsedImports actualUsedImports =
    if map prettyPrint expectUsedImports /= map prettyPrint actualUsedImports then do
        putStr $ unlines
                [ ""
                , "Expected imports:"
                , unlines $ map (toText . prettyPrint) expectUsedImports
                , "Actual imports:"
                , unlines $ map (toText . prettyPrint) actualUsedImports
                ]
        pure $ Finished $ Fail "Invalid imports"
    else
        pure $ Finished Pass

importifyFile :: FilePath -> IO (Text, [Identifier])
importifyFile filepath = do
    let args = map toText ["file", "-U", filepath]
    (exitCode, out, err) <- procStrictWithErr importifyBinaryName args empty
    case exitCode of
        ExitSuccess   -> pure (out, map (Identifier . toString) $ words err)
        ExitFailure _ -> throw $ ProcFailed { procCommand = importifyBinaryName
                                            , procArguments = args
                                            , procExitCode = exitCode
                                            }

checkImportifyExists :: IO Bool
checkImportifyExists = doesFileExist $ toString importifyBinaryName

importifyBinaryName, testDirectory :: Text
importifyBinaryName = "importify"
testDirectory = "test/system/"
