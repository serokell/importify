-- | Contains implementation of @importify file@ command.

module Importify.Main.File
       ( collectUnusedIds
       , doFile
       , doSource
       ) where

import           Universum

import           Data.Aeson             (decode)
import qualified Data.ByteString.Lazy   as BS
import qualified Data.HashMap.Strict    as HM
import qualified Data.Map               as Map

import           Language.Haskell.Exts  (Extension, ImportDecl, Module (..),
                                         ModuleName (..), SrcSpanInfo, parseExtension,
                                         prettyPrint)
import           Language.Haskell.Names (Environment, Scoped, annotate, loadBase,
                                         readSymbols)
import           Path                   (fromRelFile, parseRelDir, parseRelFile, (</>))
import           System.Directory       (doesFileExist)
import           Turtle                 (cd)

import           Importify.Cabal        (ExtensionsMap, TargetMap, moduleNameToPath)
import           Importify.Paths        (cacheDir, cachePath, extensionsFile, modulesPath,
                                         symbolsPath, targetsFile)
import           Importify.Resolution   (collectUnusedSymbols, collectUsedQuals)
import           Importify.Syntax       (Identifier, getSourceModuleName, importSlice,
                                         parseForImports)
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

            environment    <- loadEnvironment
            let annotations = annotateModule ast environment
            let unusedIds   = collectUnusedSymbols environment imports annotations
            let usedQuals   = collectUsedQuals imports annotations
            let newImports  = cleanQuals usedQuals $ removeIdentifiers unusedIds imports

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

collectUnusedIds :: Module SrcSpanInfo -> [ImportDecl SrcSpanInfo] -> IO [Identifier]
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


readExtensionMaps :: IO (Maybe (TargetMap, ExtensionsMap))
readExtensionMaps = do
    cd (fromString cacheDir)
    targetsExist    <- doesFileExist targetsFile
    extensionsExist <- doesFileExist extensionsFile
    if not (targetsExist && extensionsExist) then do
        cd ".."
        pure Nothing
    else do
        targetsMap    <- BS.readFile targetsFile
        extensionsMap <- BS.readFile extensionsFile
        cd ".."
        pure $ liftA2 (,) (decode targetsMap) (decode extensionsMap)
