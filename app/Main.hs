module Main where

import           Universum

import           Language.Haskell.Exts (ImportDecl (..), ImportSpec (..), Module (..),
                                        ParseResult (..), parseFileContents, prettyPrint)

import           Importify.Common      (Identifier (..), collectImportsList, importSlice,
                                        removeImportIdentifier)

main :: IO ()
main = do
    [fileName, id] <- getArgs
    fileContent    <- readFile fileName
    let ParseOk (Module _ _ _ imports _) = parseFileContents $ toString fileContent

    whenJust (importSlice imports) $ \(start, end) -> do
        let codeLines        = lines fileContent
        let (preamble, rest) = splitAt (start - 1) codeLines
        let (_, decls)       = splitAt (end - start + 1) rest

        let importsMap      = collectImportsList imports
        let (newImports, _) = removeImportIdentifier (Identifier id) importsMap imports

        putText $ unlines preamble
               <> toText (unlines $ map (toText . prettyPrint) newImports)
               <> unlines decls
