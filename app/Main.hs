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
import           System.Directory       (createDirectory, doesDirectoryExist)
import           Turtle                 (cd, pwd, shell)

import           Importify.Cabal        (getLibs, readCabal)
import           Importify.Cache        (cachePath)
import           Importify.Common       (collectImportsList, importSlice,
                                         removeIdentifiers)
import           Importify.Resolution   (collectUnusedSymbols)

import           Options                (CabalCacheOptions (..), Command (..),
                                         SingleFileOptions (..), parseOptions)

main :: IO ()
main = do
    opts <- parseOptions
    case opts of
        SingleFile sfOpts -> importifySingleFile sfOpts
        CabalCache ccOpts -> buildCabalCache ccOpts

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

buildCabalCache :: CabalCacheOptions -> IO ()
buildCabalCache CabalCacheOptions{..} = do
    cabalDesc <- readCabal ccoFilename
    let libs   = getLibs cabalDesc

    print libs

    unlessM (doesDirectoryExist cachePath) $ createDirectory cachePath

    -- move to cache directory and download-unpack all libs there
    cd (fromString cachePath)
    forM_ libs $ \libName -> () <$ shell ("stack unpack " <> toText libName) empty
