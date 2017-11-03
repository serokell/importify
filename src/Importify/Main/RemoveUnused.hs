{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}

-- | Contains implementation of @importify remove@ command.

module Importify.Main.RemoveUnused
       ( importifyRemoveUnused
       , importifyRemoveUnusedPath
       ) where

import           Universum

import           Fmt                                (fmt, (+|), (|+))
import           Language.Haskell.Exts              (ImportDecl, Module (..), ModuleHead,
                                                     SrcSpanInfo, exactPrint)
import           Language.Haskell.Names             (Environment, Scoped, annotate)
import           Language.Haskell.Names.Imports     (annotateImportDecls, importTable)
import           Language.Haskell.Names.SyntaxUtils (getModuleName)
import           Path                               (Abs, File, Path)
import           Path.IO                            (doesDirExist, getCurrentDir)

import           Importify.Bracket
import           Importify.OutputOptions
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

-- | Run @importify remove@ command with given options.
importifyRemoveUnused :: OutputOptions -> FilePath -> IO ()
importifyRemoveUnused = importifyOptionsBracket importifyRemoveUnusedPath

-- | Return result of @importify remove@ command on given file.
importifyRemoveUnusedPath :: Path Abs File -> IO ImportifyResult
importifyRemoveUnusedPath = importifyPathBracket (importifyAstBracket removeUnusedImports)

-- | Remove all unused entities in given module from given list of imports.
-- Algorithm performs next steps:
-- -1. Load environment
--  0. Collect annotations for module and imports.
--  1. Remove unused implicit imports.
--  2. Remove unused symbols from explicit list.
--  3. Remove unused hidings from explicit lists.
--  4. Remove unused qualified imports.
removeUnusedImports :: ImportifyFunction
removeUnusedImports ast environment imports = do
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
