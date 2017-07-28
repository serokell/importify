{-# LANGUAGE ViewPatterns #-}

-- | Contains implementation of @importify file@ command.

module Importify.Main.File
       ( OutputOptions (..)
       , doFile
       , doSource
       ) where

import           Universum

import qualified Data.HashMap.Strict                as HM
import qualified Data.Map                           as M

import           Language.Haskell.Exts              (Extension, ImportDecl, Module (..),
                                                     ModuleHead, ModuleName (..),
                                                     SrcSpanInfo, exactPrint,
                                                     fromParseResult, parseExtension,
                                                     parseFileContentsWithExts)
import           Language.Haskell.Names             (Environment, Scoped, annotate,
                                                     loadBase, readSymbols)
import           Language.Haskell.Names.Imports     (annotateImportDecls, importTable)
import           Language.Haskell.Names.SyntaxUtils (getModuleName)
import           Path                               (Abs, File, Path, fromAbsFile,
                                                     fromRelFile, parseRelDir,
                                                     parseRelFile, (</>))
import           Turtle                             (cd)

import           Importify.Cabal                    (MapBundle, TargetId (LibraryId),
                                                     TargetsMap, targetIdDir)
import           Importify.Paths                    (cacheDir, cachePath,
                                                     decodeFileOrMempty, extensionsFile,
                                                     getCurrentPath, modulesPath,
                                                     symbolsPath, targetsFile)
import           Importify.Pretty                   (printLovelyImports)
import           Importify.Resolution               (collectUnusedImplicitImports,
                                                     collectUnusedSymbolsBy, hidingUsedIn,
                                                     isKnownImport, removeImplicitImports,
                                                     removeUnusedQualifiedImports,
                                                     symbolUsedIn)
import           Importify.Syntax                   (importSlice, switchHidingImports,
                                                     unscope)
import           Importify.Tree                     (UnusedHidings (UnusedHidings),
                                                     UnusedSymbols (UnusedSymbols),
                                                     removeImports)

-- | This data type dictates how output of @importify@ should be
-- outputed.
data OutputOptions = ToConsole        -- ^ Print to console
                   | InPlace          -- ^ Change file in-place
                   | ToFile FilePath  -- ^ Print to specified file
                   deriving (Show)

doFile :: OutputOptions -> FilePath -> IO ()
doFile options srcPath = do
    src         <- readFile srcPath
    modifiedSrc <- doSource srcPath src

    case options of
        ToConsole -> putText modifiedSrc
        InPlace   -> writeFile srcPath modifiedSrc
        ToFile to -> writeFile to      modifiedSrc

doSource :: FilePath -> Text -> IO Text
doSource srcFile src = do
    mapBundle   <- readMapBundle
    srcPath     <- parseRelFile srcFile
    projectPath <- getCurrentPath
    let exts     = fromMaybe []
                 $ getExtensions (projectPath </> srcPath) mapBundle

    -- TODO: better error handling here?
    ast@(Module _ _ _ imports _) <- return  -- return here to throw exception
                                  $ fromParseResult
                                  $ parseFileContentsWithExts exts
                                  $ toString src

    case importSlice imports of
        Just (start, end) -> do
            let codeLines        = lines src
            let (preamble, rest) = splitAt (start - 1) codeLines
            let (impText, decls) = splitAt (end - start + 1) rest

            environment       <- loadEnvironment (fst mapBundle)
            let newImports     = removeUnusedImports ast imports environment
            let printedImports = printLovelyImports start end impText newImports

            return $ unlines preamble
                  <> unlines printedImports
                  <> unlines decls

        Nothing -> return src

getExtensions :: Path Abs File -> MapBundle -> Maybe [Extension]
getExtensions (fromAbsFile -> moduleFile) (targetMap, extensionsMap) = do
    target     <- HM.lookup moduleFile targetMap
    extensions <- HM.lookup target extensionsMap
    pure $ map parseExtension extensions

readMapBundle :: IO MapBundle
readMapBundle = bracket_ stepIn stepOut readBundle
  where
    stepIn  = cd (fromString cacheDir)
    stepOut = cd ".."

    readBundle = liftA2 (,)
                        (decodeFileOrMempty targetsFile    return)
                        (decodeFileOrMempty extensionsFile return)

loadEnvironment :: TargetsMap -> IO Environment
loadEnvironment targetMap = do
    baseEnvironment <- loadBase

    let cacheModuleFile = fromRelFile $ cachePath </> modulesPath
    packages <- decodeFileOrMempty cacheModuleFile $ \importsMap -> do
        let mapEntries = HM.toList importsMap
        forM mapEntries $ \(modulePath, (package, moduleName)) -> do
            packagePath     <- parseRelDir package
            symbolsFilePath <- parseRelFile $ moduleName ++ ".symbols"
            let moduleTarget = HM.lookupDefault LibraryId modulePath targetMap
            targetPath      <- parseRelDir $ toString $ targetIdDir moduleTarget
            let pathToSymbols = cachePath
                            </> symbolsPath
                            </> packagePath
                            </> targetPath
                            </> symbolsFilePath
            moduleSymbols <- readSymbols (fromRelFile pathToSymbols)
            pure (ModuleName () moduleName, moduleSymbols)

    return $ M.union baseEnvironment (M.fromList packages)

-- | Remove all unused entities in given module from given list of imports.
-- Algorithm performs next steps:
-- -1. Load environment
--  0. Collect annotations for module and imports.
--  1. Remove unused implicit imports.
--  2. Remove unused symbols from explicit list.
--  3. Remove unused hidings from explicit lists.
--  4. Remove unused qualified imports.
removeUnusedImports
    :: Module SrcSpanInfo        -- ^ Module where symbols should be removed
    -> [ImportDecl SrcSpanInfo]  -- ^ Imports from module
    -> Environment
    -> [ImportDecl SrcSpanInfo]
removeUnusedImports ast imports environment = do
    -- return exports to search for qualified imports there later
    let (annotations, moduleHead) = annotateModule ast environment

    let symbolTable    = importTable environment ast
    let hidingTable    = importTable environment $ switchHidingImports ast
    let annotatedDecls = annotateImportDecls (getModuleName ast) environment imports

    -- ordNub needed because name can occur as Qual and as UnQual
    -- but we don't care about qualification
    let unusedCollector = ordNub ... collectUnusedSymbolsBy
    let unusedSymbols   = unusedCollector (`symbolUsedIn` annotations) symbolTable
    let unusedHidings   = unusedCollector (`hidingUsedIn` annotations) hidingTable
    let unusedImplicits = collectUnusedImplicitImports (`symbolUsedIn` annotations)
                        $ filter (isKnownImport environment) annotatedDecls

    -- Remove all collected info from imports
    let withoutUnusedImplicits = removeImplicitImports unusedImplicits
                                                       annotatedDecls
    let withoutUnusedSymbols   = map unscope
                               $ removeImports (UnusedSymbols unusedSymbols)
                                               (UnusedHidings unusedHidings)
                                               withoutUnusedImplicits
    let withoutUnusedQuals     = removeUnusedQualifiedImports withoutUnusedSymbols
                                                              moduleHead
                                                              annotations
                                                              unusedImplicits

    withoutUnusedQuals

-- | Annotates module but drops import annotations because they can contain GlobalSymbol
-- annotations and collectUnusedSymbols later does its job by looking for GlobalSymbol
annotateModule :: Module SrcSpanInfo
               -> Environment
               -> ([Scoped SrcSpanInfo], Maybe (ModuleHead SrcSpanInfo))
annotateModule ast environment =
    let (Module l mhead mpragmas _mimports mdecls) = annotate environment ast
    in (toList (Module l mhead mpragmas [] mdecls), fmap unscope mhead)
