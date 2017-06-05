{-| Tool for managing import sections.

    Remove redundant imports algorithm (current version):
      1. For every import declaration that in @baseEnvironment@
         traverse list of import names and collect those that are not in module.
      2. Remove every name from corresponding imports lists.
      3. Print new modified version of file with imports changed.
 -}

module Main where

import           Universum

import           Data.Aeson             (decode, encode)
import qualified Data.ByteString.Lazy   as BS
import qualified Data.HashMap.Strict    as Map

import           Language.Haskell.Exts  (Extension, Module (..), ParseResult (..),
                                         fromParseResult, parseExtension, parseFile,
                                         parseFileContents, parseFileContentsWithExts,
                                         prettyPrint)
import           Language.Haskell.Names (annotate, loadBase, writeSymbols)
import           Path                   (Dir, Rel, filename, fromAbsDir, fromAbsFile,
                                         fromRelFile, parseAbsDir, parseRelDir,
                                         parseRelFile, (</>))
import           Path.Internal          (Path (..))
import           System.Directory       (createDirectory, createDirectoryIfMissing,
                                         doesDirectoryExist, doesFileExist,
                                         getCurrentDirectory, listDirectory,
                                         removeDirectoryRecursive)
import           Turtle                 (cd, pwd, shell)

import           Importify.Cabal        (ExtensionsMap, TargetMap, getExtensionMaps,
                                         getLibs, getLibs, moduleNameToPath, modulePaths,
                                         readCabal, readCabal, withLibrary)
import           Importify.Cache        (cacheDir, cachePath, guessCabalName, symbolsDir,
                                         symbolsPath)
import           Importify.Common       (collectImportsList, getModuleName, importSlice,
                                         removeIdentifiers)
import           Importify.CPP          (withModuleAST)
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
    let moduleName = getModuleName fileContent
    extensionMaps <- readExtensionMaps
    let exts = fromMaybe [] $ getExtensions moduleName extensionMaps
    let ast@(Module _ _ _ imports _) = fromParseResult $ parseFileContentsWithExts exts
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

getExtensions :: String -> Maybe (TargetMap, ExtensionsMap) -> Maybe [Extension]
getExtensions moduleName maps = do
    (targetMap, extensionsMap) <- maps
    let modulePath = moduleNameToPath moduleName
    target <- Map.lookup modulePath targetMap
    extensions <- Map.lookup target extensionsMap
    pure $ map parseExtension extensions

buildCabalCache :: CabalCacheOptions -> IO ()
buildCabalCache CabalCacheOptions{..} = do
    cabalDesc <- readCabal ccoFilename

    curDir           <- getCurrentDirectory
    projectPath      <- parseAbsDir curDir
    let importifyPath = projectPath </> cachePath
    let importifyDir  = fromAbsDir importifyPath

    createDirectoryIfMissing True importifyDir  -- creates ./.importify
    cd $ fromString cacheDir    -- cd to ./.importify/

    -- Extension maps
    let (targetMaps, extensionMaps) = getExtensionMaps cabalDesc
    BS.writeFile targetsMapFilename    $ encode targetMaps
    BS.writeFile extensionsMapFilename $ encode extensionMaps

    -- Libraries
    let libs = getLibs cabalDesc
    print libs

    -- download & unpack sources, then cache and delete
    forM_ (filter (/= "base") libs) $ \libName -> do -- TODO: temp hack
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
        withLibrary packageCabalDesc $ \library cabalExtensions -> do
            modPaths <- modulePaths packagePath library
            forM_ modPaths $ \modPath -> withModuleAST modPath cabalExtensions $ \moduleAST -> do
                let resolvedSymbols  = resolveOneModule moduleAST
                modSymbolsPath      <- parseRelFile $ fromRelFile (filename modPath) ++ ".symbols"
                let packageCachePath = symbolsCachePath </> packagePath
                let moduleCachePath  = packageCachePath </> modSymbolsPath

                -- creates ./.importify/symbols/<package>/<Module.Name>.symbols
                createDirectoryIfMissing True $ fromAbsDir packageCachePath
                writeSymbols (fromAbsFile moduleCachePath) resolvedSymbols

        removeDirectoryRecursive downloadedPackage -- TODO: use bracket here

    cd ".."

readExtensionMaps :: IO (Maybe (TargetMap, ExtensionsMap))
readExtensionMaps = do
    cd (fromString cacheDir)
    targetsExist    <- doesFileExist targetsMapFilename
    extensionsExist <- doesFileExist extensionsMapFilename
    if not (targetsExist && extensionsExist) then do
        cd ".."
        pure Nothing
    else do
        targetsFile    <- BS.readFile targetsMapFilename
        extensionsFile <- BS.readFile extensionsMapFilename
        cd ".."
        pure $ liftA2 (,) (decode targetsFile) (decode extensionsFile)

targetsMapFilename :: String
targetsMapFilename = "targets"

extensionsMapFilename :: String
extensionsMapFilename = "extensions"
