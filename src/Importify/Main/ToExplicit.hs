-- | Contains command to convert implicit imports to explicit.

module Importify.Main.ToExplicit
       ( importifyToExplicit
       , importifyToExplicitPath
       ) where

import Universum

import Data.List (partition)
import Language.Haskell.Exts
import Language.Haskell.Names
import Language.Haskell.Names.Imports
import Language.Haskell.Names.SyntaxUtils
import Path

import Extended.Data.List
import Extended.Data.Map
import Importify.Bracket
import Importify.OutputOptions
import Importify.Resolution
import Importify.Syntax

import qualified Data.Map as Map
import qualified Data.Set as Set

-- | Function for @importify to-explicit@ command.
importifyToExplicit :: OutputOptions -> FilePath -> IO ()
importifyToExplicit = importifyOptionsBracket importifyToExplicitPath

-- | Return result of @importify to-explicit@ command on given file.
importifyToExplicitPath :: Path Abs File -> IO ImportifyResult
importifyToExplicitPath = importifyPathBracket (importifyAstBracket implicitToExplicitImports)

implicitToExplicitImports :: ImportifyFunction
implicitToExplicitImports ast environment imports = do
    -- Split all imports into implicit and explicit
    let (implicitImports, explicitImports) = partition isImportImplicit imports

    -- Annotate imports to check where each Symbol came from
    let symbolTable    = importTable environment ast
    let annotatedDecls = annotateImportDecls (getModuleName ast) environment implicitImports

    -- Collect list of all symbols that are imported from implicit imports
    let implicitSymbols = concatNub importedSymbols annotatedDecls

    -- Collect annotated symbols from AST that needed to be imported
    let (annotations, _) = annotateModule ast environment
    let symbols          = ordNub
                         $ foldMaybeMap (shouldBeImported . scopedNameInfo) annotations
    let neededToImport   = filter (isIn implicitSymbols) symbols
    debugAST "SYMBOLS" symbols
    debugAST "TO IMPS" neededToImport

    -- Build mapping from module names to symbols they import;
    -- also from symbol to module this symbol came from
    let moduleToImport :: Map (ModuleName ()) (InScoped ImportDecl)
        moduleToImport = mapBy getImportModuleName annotatedDecls
    let importModulesSet :: Set (ModuleName ())
        importModulesSet = Map.keysSet moduleToImport
    let importModuleToSymbols :: Map (ModuleName ()) [Symbol]
        importModuleToSymbols = fmap importedSymbols moduleToImport
    let symbolToImportModule :: Map Symbol (ModuleName ())
        symbolToImportModule = invertMapToList importModuleToSymbols

    debugAST "IMPLICIT NAMES" importModulesSet
    debugAST "SYMBOLS TO IMPORTS" symbolToImportModule

    -- Collect mapping from module names to symbols they should import
    let toImport :: Map (ModuleName ()) (NonEmpty Symbol)
        toImport = foldl' (locateImports importModulesSet symbolToImportModule) mempty neededToImport

    debugAST "COLLECTED" toImport

    return imports

shouldBeImported :: NameInfo l -> Maybe [Symbol]
shouldBeImported nameInfo = shouldBeGlobalSymbol
                        <|> shouldBeRecPatWildcard
  where
    shouldBeGlobalSymbol :: Maybe [Symbol]
    shouldBeGlobalSymbol = do
        GlobalSymbol symbol _ <- Just nameInfo
        pure [symbol]

    shouldBeRecPatWildcard :: Maybe [Symbol]
    shouldBeRecPatWildcard = do
        RecPatWildcard symbols <- Just nameInfo
        pure symbols

locateImports :: Set (ModuleName ())
              -> Map Symbol (ModuleName ())
              -> Map (ModuleName ()) (NonEmpty Symbol)
              -> Symbol
              -> Map (ModuleName ()) (NonEmpty Symbol)
locateImports modules symbolToImportModule namesToSymbols symbol =
    let moduleName = symbolModule symbol in
    case Set.member moduleName modules of
        True  -> insertNonEmpty moduleName symbol namesToSymbols
        False -> case Map.lookup symbol symbolToImportModule of
            Nothing   -> namesToSymbols
            Just name -> insertNonEmpty name symbol namesToSymbols
