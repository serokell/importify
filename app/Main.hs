{-| Tool for managing import sections.

    Remove redundant imports algorithm (current version):
      1. For every import declaration that in @baseEnvironment@
         traverse list of import names and collect those that are not in module.
      2. Remove every name from corresponding imports lists.
      3. Print new modified version of file with imports changed.
 -}

module Main where

import           Universum

import           Language.Haskell.Exts  (Module (..), ParseResult (..), fromParseResult,
                                         parseFile, parseFileContents, prettyPrint)
import           Language.Haskell.Names (annotate, loadBase, writeSymbols)
import           Path                   (Dir, Rel, filename, fromAbsDir, fromAbsFile,
                                         fromRelFile, parseAbsDir, parseRelDir,
                                         parseRelFile, (</>))
import           Path.Internal          (Path (..))
import           System.Directory       (createDirectory, createDirectoryIfMissing,
                                         doesDirectoryExist, getCurrentDirectory,
                                         listDirectory, removeDirectoryRecursive)
import           Turtle                 (cd, pwd, shell)

import           Importify.Cabal        (getLibs, modulePaths, readCabal, withLibrary)
import           Importify.Cache        (cacheDir, cachePath, guessCabalName, symbolsDir,
                                         symbolsPath)
import           Importify.Common       (collectImportsList, importSlice,
                                         removeIdentifiers)
import           Importify.Resolution   (collectUnusedSymbols, resolveOneModule)

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

    curDir           <- getCurrentDirectory
    projectPath      <- parseAbsDir curDir
    let importifyPath = projectPath </> cachePath
    let importifyDir  = fromAbsDir importifyPath

    createDirectoryIfMissing True importifyDir  -- creates ./.importify

    cd $ fromString cacheDir    -- cd to ./.importify/
    forM_ libs $ \libName -> do -- download & unpack sources, then cache and delete
        _exitCode            <- shell ("stack unpack " <> toText libName) empty
        localPackages        <- listDirectory importifyDir
        let maybePackage      = find (libName `isPrefixOf`) localPackages
        let downloadedPackage = fromMaybe (error "Package wasn't downloaded!")
                                          maybePackage  -- TODO: this is not fine

        packagePath      <- parseRelDir downloadedPackage
        let cabalFileName = guessCabalName libName
        packageCabalDesc <- readCabal $ fromAbsFile
                                      $ importifyPath </> packagePath </> cabalFileName

        let symbolsCachePath = importifyPath </> symbolsPath
        withLibrary packageCabalDesc $ \library -> do
            modPaths <- modulePaths packagePath library
            forM_ modPaths $ \modPath -> do
                -- TODO: path default extensions from .cabal file here
                putStrLn $ "Current module: " ++ fromRelFile modPath
                ParseOk m <- parseFile $ fromRelFile modPath
                let resolvedSymbols  = resolveOneModule m
                modSymbolsPath      <- parseRelFile $ fromRelFile (filename modPath) ++ ".symbols"
                let packageCachePath = symbolsCachePath </> packagePath
                let moduleCachePath  = packageCachePath </> modSymbolsPath

                -- creates ./.importify/symbols/<package>/<Module.Name>.symbols
                createDirectoryIfMissing True $ fromAbsDir packageCachePath
                writeSymbols (fromAbsFile moduleCachePath) resolvedSymbols

        removeDirectoryRecursive downloadedPackage -- TODO: use bracket here
