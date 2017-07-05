-- | Contains implementation of @importify file@ command.

module Importify.Main.File
       ( collectAndRemoveUnusedSymbols
       , doFile
       , doSource
       ) where

import           Universum

import           Data.Aeson                         (decode)
import qualified Data.ByteString.Lazy               as BS
import qualified Data.HashMap.Strict                as HM
import           Data.List                          ((\\))
import qualified Data.Map                           as Map
import           Data.Maybe                         (fromJust)

import           Language.Haskell.Exts              (Extension, ImportDecl (importModule, importSpecs),
                                                     Module (..), ModuleName (..),
                                                     Name (..), QName (..), SrcSpanInfo,
                                                     exactPrint, parseExtension,
                                                     prettyPrint)
import           Language.Haskell.Names             (Environment, Scoped, annotate,
                                                     loadBase, readSymbols, symbolName)

import qualified Language.Haskell.Names             as N
import           Language.Haskell.Names.Imports     (annotateImportDecls, importTable)
import           Language.Haskell.Names.SyntaxUtils (getModuleName, stringToName)
import           Path                               (fromRelFile, parseRelDir,
                                                     parseRelFile, (</>))
import           System.Directory                   (doesFileExist)
import           Turtle                             (cd)

import           Importify.Cabal                    (ExtensionsMap, TargetMap,
                                                     moduleNameToPath)
import           Importify.Paths                    (cacheDir, cachePath, extensionsFile,
                                                     modulesPath, symbolsPath,
                                                     targetsFile)
import           Importify.Pretty                   (printLovelyImports)
import           Importify.Resolution               (collectUnusedSymbols,
                                                     removeUnusedQualifiedAsImports)
import           Importify.Syntax                   (debugAST, getModuleNameId,
                                                     getSourceModuleName, importSlice,
                                                     parseForImports, unscope)
import           Importify.Tree                     (removeSymbols)

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
            let (impText, decls) = splitAt (end - start + 1) rest

            newImports          <- collectAndRemoveUnusedSymbols ast imports
            let printedImports   = printLovelyImports start end impText newImports

            pure $ unlines preamble
                <> unlines printedImports
                <> unlines decls

        Nothing -> pure src

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

-- | Collect all unused entities in given module from given list of imports.
collectAndRemoveUnusedSymbols
    :: Module SrcSpanInfo        -- ^ Module where symbols should be removed
    -> [ImportDecl SrcSpanInfo]  -- ^ Imports from module
    -> IO [ImportDecl SrcSpanInfo]
collectAndRemoveUnusedSymbols ast imports = do
    environment       <- loadEnvironment
    let table          = importTable environment ast
    let annotatedDecls = annotateImportDecls (getModuleName ast) environment imports
    let annotations    = annotateModule ast environment

    -- ordNub needed because name can occur as Qual and as UnQual
    -- but we don't care about qualification
    let unusedSymbols        = ordNub $ collectUnusedSymbols annotations table
    let withoutUnusedSymbols = map unscope $ removeSymbols unusedSymbols annotatedDecls
    let withoutUnusedQualsAs = removeUnusedQualifiedAsImports withoutUnusedSymbols
                                                              annotations

    return withoutUnusedQualsAs

loadEnvironment :: IO Environment
loadEnvironment = do
    base <- loadBase

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
