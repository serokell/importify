{-# LANGUAGE RecordWildCards #-}

{-| Tool for managing import sections.

    Remove redundant imports algorithm (current version):
      1. For every import declaration that in @baseEnvironment@
         traverse list of import names and collect those that are not in module.
      2. Remove every name from corresponding imports lists.
      3. Print new modified version of file with imports changed.
 -}

module Main where

import           Universum

import           Language.Haskell.Exts  (Module (..), fromParseResult, parseFileContents,
                                         prettyPrint)
import           Language.Haskell.Names (annotate, loadBase)

import           Importify.Common       (collectImportsList, importSlice,
                                         removeIdentifiers)
import           Importify.Resolution   (collectUnusedSymbols)

import           Importify.Options      (Command (..), SingleFileOptions (..),
                                         parseOptions)

main :: IO ()
main = do
    opts <- parseOptions
    case opts of
        SingleFile sfOpts -> importifySingleFile sfOpts

importifySingleFile :: SingleFileOptions -> IO ()
importifySingleFile SingleFileOptions{..} = do
    fileContent <- readFile sfoFilename
    let ast@(Module _ _ _ imports _) = fromParseResult $ parseFileContents
                                                       $ toString fileContent

    whenJust (importSlice imports) $ \(start, end) -> do
        let codeLines        = lines fileContent
        let (preamble, rest) = splitAt (start - 1) codeLines
        let (_, decls)       = splitAt (end - start + 1) rest

        baseEnvironment <- loadBase
        let annotatedAST = annotate baseEnvironment ast
        let annotations  = toList annotatedAST
        let unusedIds    = collectUnusedSymbols baseEnvironment imports annotations

        let importsMap = collectImportsList imports
        let newImports = removeIdentifiers unusedIds importsMap imports

        putText $ unlines preamble
               <> toText (unlines $ map (toText . prettyPrint) newImports)
               <> unlines decls
