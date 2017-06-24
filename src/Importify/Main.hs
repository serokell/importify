module Importify.Main
       ( doFile
       , doSource
       , doCache
       , collectUnusedIds
       ) where

import           Universum

import           Data.Aeson             (decode, encode)
import qualified Data.ByteString.Lazy   as BS
import qualified Data.HashMap.Strict    as HM
import qualified Data.Map               as Map

import           Language.Haskell.Exts  (Extension, ImportDecl, Module (..),
                                         ModuleName (..), SrcSpanInfo, parseExtension,
                                         prettyPrint)
import           Language.Haskell.Names (Environment, Scoped, annotate, loadBase,
                                         readSymbols, writeSymbols)
import           Path                   (filename, fromAbsDir, fromAbsFile, fromRelFile,
                                         parseAbsDir, parseRelDir, parseRelFile, (</>))
import           System.Directory       (createDirectoryIfMissing, doesFileExist,
                                         getCurrentDirectory, listDirectory,
                                         removeDirectoryRecursive)
import           System.FilePath        (dropExtension, takeFileName)
import           Turtle                 (cd, shell)

import           Importify.Cabal        (ExtensionsMap, TargetMap, getExtensionMaps,
                                         getLibs, getLibs, moduleNameToPath, modulePaths,
                                         readCabal, readCabal, splitOnExposedAndOther,
                                         withLibrary)
import           Importify.Cache        (cacheDir, cachePath, guessCabalName, modulesFile,
                                         modulesPath, symbolsPath)
import           Importify.Common       (Identifier, getModuleTitle, getSourceModuleName,
                                         importSlice, parseForImports)
import           Importify.CPP          (parseModuleFile)
import           Importify.Resolution   (collectUnusedSymbols, collectUsedQuals,
                                         resolveModules)
import           Importify.Tree         (cleanQuals, removeIdentifiers)

doFile :: FilePath -> IO ()
doFile = readFile >=> doSource >=> putText

doSource :: Text -> IO Text
doSource src = do
    let moduleName = getSourceModuleName src
    extensionMaps <- readExtensionMaps
    let exts = fromMaybe [] $ getExtensions moduleName extensionMaps
    let (ast, imports) = parseForImports exts src

    case importSlice imports of
        Just (start, end) -> do
            let codeLines        = lines src
            let (preamble, rest) = splitAt (start - 1) codeLines
            let (_, decls)       = splitAt (end - start + 1) rest

            environment <- loadEnvironment
            let annotations = annotateModule ast environment
            let unusedIds = collectUnusedSymbols environment imports annotations
            let usedQuals = collectUsedQuals imports annotations
            let newImports = cleanQuals usedQuals $ removeIdentifiers unusedIds imports

            pure $ unlines preamble
                <> toText (unlines $ map (toText . prettyPrint) newImports)
                <> unlines decls
        Nothing -> pure src

loadEnvironment :: IO Environment
loadEnvironment = do
    base <- loadBase

    -- TODO: change after refactoring files variables
    let cacheModuleFile = fromRelFile $ cachePath </> modulesPath
    isImportsExist <- doesFileExist cacheModuleFile

    packages <- if isImportsExist then do
                  importsMap    <- decode <$> BS.readFile cacheModuleFile
                  let mapEntries = HM.toList $ fromMaybe (error "imports decoding failed")
                                                          importsMap
                  forM mapEntries $ \(moduleName, package) -> do
                      packagePath <- parseRelDir package
                      modulePath  <- parseRelFile $ moduleName ++ ".symbols"
                      let pathToSymbols = cachePath
                                      </> symbolsPath
                                      </> packagePath
                                      </> modulePath
                      moduleSymbols <- readSymbols (fromRelFile pathToSymbols)
                      pure (ModuleName () moduleName, moduleSymbols)
                else
                  pure mempty

    return $ Map.union base (Map.fromList packages)

-- | Annotates module but drops import annotations because they can contain GlobalSymbol
-- annotations and collectUnusedSymbols later does its job by looking for GlobalSymbol
annotateModule :: Module SrcSpanInfo -> Environment -> [Scoped SrcSpanInfo]
annotateModule ast environment =
    let (Module l mhead mpragmas _mimports mdecls) = annotate environment ast
    in toList (Module l mhead mpragmas [] mdecls)

collectUnusedIds :: Module SrcSpanInfo -> [ImportDecl SrcSpanInfo] -> IO ([Identifier])
collectUnusedIds ast imports = do
    environment <- loadEnvironment
    let annotations = annotateModule ast environment
    pure $ collectUnusedSymbols environment imports annotations

getExtensions :: String -> Maybe (TargetMap, ExtensionsMap) -> Maybe [Extension]
getExtensions moduleName maps = do
    (targetMap, extensionsMap) <- maps
    let modulePath = moduleNameToPath moduleName
    target <- HM.lookup modulePath targetMap
    extensions <- HM.lookup target extensionsMap
    pure $ map parseExtension extensions

-- | Caches packages information into local .importify directory.
doCache :: FilePath -> Bool -> IO ()
doCache filepath preserve = do
    cabalDesc <- readCabal filepath

    curDir           <- getCurrentDirectory
    projectPath      <- parseAbsDir curDir
    let importifyPath = projectPath </> cachePath
    let importifyDir  = fromAbsDir importifyPath

    -- TODO: error if not inside project directory?
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
    let projectName = dropExtension $ takeFileName filepath
    modulesToPackage <- forM (filter (\p -> p /= "base" && p /= projectName) libs) $ \libName -> do
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
        packageModules <- withLibrary packageCabalDesc $ \library cabalExtensions -> do
            -- creates ./.importify/symbols/<package>/
            let packageCachePath = symbolsCachePath </> packagePath
            createDirectoryIfMissing True $ fromAbsDir packageCachePath

            modPaths   <- modulePaths packagePath library
            modEithers <- mapM (parseModuleFile cabalExtensions) modPaths
            let (errors, libModules) = partitionEithers modEithers

            whenNotNull errors $ \messages -> do
                putText $ " * Next errors occured during caching of package: " <> toText libName
                for_ messages putText

            let (exposedModules, otherModules) = splitOnExposedAndOther library libModules
            let resolvedModules = resolveModules exposedModules otherModules

            forM resolvedModules $ \(ModuleName () moduleTitle, resolvedSymbols) -> do
                modSymbolsPath     <- parseRelFile $ moduleTitle ++ ".symbols"
                let moduleCachePath = packageCachePath </> modSymbolsPath

                -- creates ./.importify/symbols/<package>/<Module.Name>.symbols
                writeSymbols (fromAbsFile moduleCachePath) resolvedSymbols

                pure (moduleTitle, downloadedPackage)

        unless preserve $  -- TODO: use bracket here
            removeDirectoryRecursive downloadedPackage

        pure $ Map.fromList packageModules

    let importsMap = Map.unions modulesToPackage
    BS.writeFile modulesFile $ encode importsMap

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
