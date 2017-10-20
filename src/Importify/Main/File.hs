{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts      #-}

-- | Contains implementation of @importify file@ command.

module Importify.Main.File
       ( OutputOptions (..)
       , importifyFileOptions
       , importifyFileContent
       ) where

import           Universum

import qualified Data.HashMap.Strict                as HM
import qualified Data.Map                           as M

import           Fmt                                (fmt, (+|), (|+))
import           Language.Haskell.Exts              (Extension, ImportDecl, Module (..),
                                                     ModuleHead, ModuleName (..),
                                                     SrcSpanInfo, exactPrint,
                                                     parseExtension,
                                                     parseFileContentsWithExts)
import           Language.Haskell.Names             (Environment, Scoped, annotate,
                                                     loadBase, readSymbols)
import           Language.Haskell.Names.Imports     (annotateImportDecls, importTable)
import           Language.Haskell.Names.SyntaxUtils (getModuleName)
import           Path                               (Abs, Dir, File, Path, Rel,
                                                     fromAbsFile, fromRelFile,
                                                     parseRelDir, parseRelFile, (</>))
import           Path.IO                            (doesDirExist, getCurrentDir)

import           Extended.System.Wlog               (printError, printNotice)
import           Importify.Cabal                    (ExtensionsMap, ModulesBundle (..),
                                                     ModulesMap, TargetId, targetIdDir)
import           Importify.ParseException           (eitherParseResult, setMpeFile)
import           Importify.Path                     (decodeFileOrMempty, doInsideDir,
                                                     extensionsPath, importifyPath,
                                                     lookupToRoot, modulesPath,
                                                     symbolsPath)
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

newtype ImportifyFileException = IFE Text

-- | Run @importify file@ command with given options.
importifyFileOptions :: OutputOptions -> FilePath -> IO ()
importifyFileOptions options srcFile = do
    srcPath   <- parseRelFile srcFile
    foundRoot <- lookupToRoot (doesDirExist . (</> importifyPath)) srcPath
    case foundRoot of
        Nothing ->
            printError "Directory '.importify' is not found. Either cache for project \
                       \is not created or not running from project directory."
        Just (rootDir, srcFromRootPath) -> do
            curDir          <- getCurrentDir
            importifyResult <- doInsideDir rootDir (importifyFileContent $ curDir </> srcFromRootPath)
            handleOptions importifyResult
  where
    handleOptions :: Either ImportifyFileException Text -> IO ()
    handleOptions (Left (IFE msg))    = printError msg
    handleOptions (Right modifiedSrc) = case options of
        ToConsole -> putText modifiedSrc
        InPlace   -> writeFile srcFile modifiedSrc
        ToFile to -> writeFile to      modifiedSrc

-- | Return result of @importify file@ command.
importifyFileContent :: Path Abs File -> IO (Either ImportifyFileException Text)
importifyFileContent srcPath = do
    let srcFile    = fromAbsFile srcPath

    modulesMap <- readModulesMap
    extensions <- readExtensions srcPath modulesMap

    whenNothing_ (HM.lookup (fromAbsFile srcPath) modulesMap) $
        printNotice $ "File '"+|srcFile|+"' is not cached: new file or caching error"

    src <- readFile srcFile
    let parseResult = eitherParseResult
                    $ parseFileContentsWithExts extensions
                    $ toString src

    case parseResult of
        Left exception -> return $ Left $ IFE $ setMpeFile srcFile exception |+ ""
        Right ast      -> importifyAst src modulesMap ast

importifyAst :: Text
             -> ModulesMap
             -> Module SrcSpanInfo
             -> IO (Either ImportifyFileException Text)
importifyAst src modulesMap ast@(Module _ _ _ imports _) =
    Right <$> case importSlice imports of
        Nothing           -> return src
        Just (start, end) -> do
            let codeLines        = lines src
            let (preamble, rest) = splitAt (start - 1) codeLines
            let (impText, decls) = splitAt (end - start + 1) rest

            environment       <- loadEnvironment modulesMap
            let newImports     = removeUnusedImports ast imports environment
            let printedImports = printLovelyImports start end impText newImports

            return $ unlines preamble
                  <> unlines printedImports
                  <> unlines decls
importifyAst _ _ _ = return $ Left $ IFE "Module wasn't parsed correctly"

readModulesMap :: IO ModulesMap
readModulesMap = decodeFileOrMempty (importifyPath </> modulesPath) pure

readExtensions :: Path Abs File -> ModulesMap -> IO [Extension]
readExtensions srcPath modulesMap = do
    case HM.lookup (fromAbsFile srcPath) modulesMap of
        Nothing                -> return []
        Just ModulesBundle{..} -> do
            packagePath <- parseRelDir $ toString mbPackage
            projectPath <- getCurrentDir
            let pathToExtensions = projectPath
                               </> importifyPath
                               </> symbolsPath
                               </> packagePath
                               </> extensionsPath

            let lookupExtensions = fromMaybe [] . getExtensions mbTarget
            decodeFileOrMempty @ExtensionsMap
                               pathToExtensions
                               (return . lookupExtensions)

getExtensions :: TargetId -> ExtensionsMap -> Maybe [Extension]
getExtensions targetId = fmap (map parseExtension) . HM.lookup targetId

loadEnvironment :: ModulesMap -> IO Environment
loadEnvironment modulesMap = do
    baseEnvironment <- loadBase

    let moduleBundles = HM.elems modulesMap
    packages <- forM moduleBundles $ \ModulesBundle{..} -> do
        packagePath     <- parseRelDir  $ toString mbPackage
        symbolsFilePath <- parseRelFile $ mbModule ++ ".symbols"
        targetPath      <- parseRelDir $ toString $ targetIdDir mbTarget
        let pathToSymbols = importifyPath
                        </> symbolsPath
                        </> packagePath
                        </> targetPath
                        </> symbolsFilePath
        moduleSymbols <- readSymbols (fromRelFile pathToSymbols)
        pure (ModuleName () mbModule, moduleSymbols)

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
