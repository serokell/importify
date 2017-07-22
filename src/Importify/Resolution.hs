{-# LANGUAGE ExplicitForAll      #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns        #-}

-- | This module contains functions to work with name resolution.

module Importify.Resolution
       ( -- * Unused symbols search engines
         collectUnusedImplicitImports
       , collectUnusedSymbolsBy
       , isKnownImport

         -- * Predicates for unused imports
       , hidingUsedIn
       , symbolUsedIn

         -- * Removals
       , removeImplicitImports
       , removeUnusedQualifiedImports

         -- * Resolvers
       , resolveModules
       ) where

import           Universum

import           Data.Data                                (Data)
import           Data.List                                (notElem, partition)
import qualified Data.Map.Strict                          as M

import           Language.Haskell.Exts                    (ExportSpec (EModuleContents),
                                                           ExportSpecList (..),
                                                           ImportDecl (..), Module,
                                                           ModuleHead (..),
                                                           ModuleName (..), QName (..))
import           Language.Haskell.Names                   (Environment,
                                                           NameInfo (Export, GlobalSymbol),
                                                           Scoped, resolve, symbolModule)
import qualified Language.Haskell.Names                   as N (Symbol (..), symbolName)
import           Language.Haskell.Names.GlobalSymbolTable (Table)
import           Language.Haskell.Names.SyntaxUtils       (dropAnn, getModuleName)

import           Importify.Syntax                         (InScoped, getImportModuleName,
                                                           importNamesWithTables,
                                                           isImportImplicit,
                                                           scopedNameInfo, scopedNameInfo)

elemAnnotations :: (NameInfo l -> Bool) -> [Scoped l] -> Bool
elemAnnotations used = any used . map scopedNameInfo

-- | Checks if 'Symbol' is used inside annotations. This function
-- needed to remove unused imports.
symbolUsedIn :: N.Symbol -> [Scoped l] -> Bool
symbolUsedIn symbol = elemAnnotations used
  where
    used :: NameInfo l -> Bool

    -- Constructors are special because the whole type should be considered used
    -- if one of its constructors is used
    used (GlobalSymbol global@(N.Constructor smodule _sname stype) _) =
        symbol == global ||
        (N.symbolName symbol == stype && symbolModule symbol == smodule)

    -- ditto for selectors
    used (GlobalSymbol global@(N.Selector smodule _sname stype _scons) _) =
        symbol == global ||
        (N.symbolName symbol == stype && symbolModule symbol == smodule)

    -- Symbol is used as a part of export declaration
    used (Export symbols) = symbol `elem` symbols

    -- The symbol is used itself
    used (GlobalSymbol global _) = symbol == global
    used _                       = False

-- | Checks if given 'Symbol' is used in module annotations. This
-- function performs comparison by ignoring module names because we want
-- to remove @hiding@ by calling this function.
hidingUsedIn :: N.Symbol -> [Scoped l] -> Bool
hidingUsedIn symbol = elemAnnotations used
  where
    used :: NameInfo l -> Bool
    used (GlobalSymbol global _) = modulelessEq symbol global
    used (Export symbols)        = any (modulelessEq symbol) symbols
    used _                       = False

-- | Compares if two symbols are equal ignoring 'symbolModule'
-- field. Used to remove imports from @hiding@ sections.
modulelessEq :: N.Symbol -> N.Symbol -> Bool
modulelessEq this other = this { N.symbolModule = N.symbolModule other } == other

-- | Collect symbols unused in annotations.
collectUnusedSymbolsBy
    :: (N.Symbol -> Bool) -- ^ 'True' iff 'Symbol' is used
    -> Table              -- ^ Mapping from imported names to their symbols
    -> [N.Symbol]         -- ^ Returns list of unused symbols from 'Table'
collectUnusedSymbolsBy isUsed table = do
    -- 1. For every pair (entity, its symbols) in Table
    (_, importedSymbols) <- M.toList table

    -- 2. And for every entity with same name
    symbol <- importedSymbols

    -- 3. Check whether this symbol used or not
    guard $ not $ isUsed symbol

    -- 4. If not found ⇒ unused
    pure symbol

-- | Collect names of unused implicit imports.
collectUnusedImplicitImports :: (N.Symbol -> Bool)
                             -> [InScoped ImportDecl]
                             -> [ModuleName ()]
collectUnusedImplicitImports isUsed imports =
    let implicitImports = filter isImportImplicit imports
        nameWithTable   = importNamesWithTables implicitImports
        isImportUnused  = null . collectUnusedSymbolsBy (not . isUsed)
        unusedImports   = map fst $ filter (isImportUnused . snd) nameWithTable
    in unusedImports

-- | Checks if module symbols were cached. We don't want to remove
-- unknown imports we just want to not touch them.
isKnownImport :: Environment -> ImportDecl l -> Bool
isKnownImport env decl = M.member (getImportModuleName decl) env

-- | Remove all implicit import declarations specified by given list
-- of module names.
removeImplicitImports :: [ModuleName ()]
                      -> [ImportDecl l]
                      -> [ImportDecl l]
removeImplicitImports names = filter notImplicitOrUnused
  where
    notImplicitOrUnused imp@ImportDecl{..} = not (isImportImplicit imp)
                                          || dropAnn importModule `notElem` names

-- | Gather all symbols for given list of 'Module's. In reality those
-- modules represent all /exposed/ and /other/ modules for one package
-- returning only list of symbols for /exposed/ modules.
resolveModules :: (Data l, Eq l) => [Module l] -> [Module l] -> [(ModuleName (), [N.Symbol])]
resolveModules exposedModules otherModules =
    let symbolsEnv     = resolve (exposedModules ++ otherModules) mempty
        otherCleared   = map ((() <$) . getModuleName) otherModules

        -- remove @otherModules@ from environment because only @exposed@ can be imported
        exposedEnv     = foldl' (flip M.delete) symbolsEnv otherCleared
        exposedSymbols = M.assocs exposedEnv
    in exposedSymbols

-- | Remove unused @qualified as@ imports, i.e. in one of the next form:
-- @
--   import qualified Data.List
--   import qualified Data.List as L
--   import           Data.List as L
-- @
-- This function ignores qualified imports because it is running after
-- stage where symbols from explicit list removed.
removeUnusedQualifiedImports :: [ImportDecl l]
                             -> Maybe (ModuleHead l)
                             -> [Scoped l]
                             -> [ImportDecl l]
removeUnusedQualifiedImports imports moduleHead annotations =
    let (emptySpecs, others) = partition (isNothing . importSpecs) imports
        isImportNeeded name  = isInsideExport moduleHead  name
                            || isInsideModule annotations name
        byModuleName         = maybe True isImportNeeded . fmap dropAnn . qualifiedName
        neededQualified      = filter byModuleName emptySpecs
    in neededQualified ++ others

-- | For given import collect qualified name.
-- Qualified names gathered using next scheme:
-- @
--   import           A      ⇒ Nothing
--   import qualified B      ⇒ Just B
--   import qualified C as X ⇒ Just X
--   import           D as Y ⇒ Just Y
-- @
-- Used later to determine whether empty @qualified@ import needed or not.
qualifiedName :: ImportDecl l -> Maybe (ModuleName l)
qualifiedName ImportDecl{ importAs = as@(Just _)     } = as
qualifiedName ImportDecl{ importQualified = True, .. } = Just importModule
qualifiedName _                                        = Nothing

isInsideExport :: forall l. Maybe (ModuleHead l) -> ModuleName () -> Bool
isInsideExport moduleHead moduleName = moduleName `elem` exportedModules
  where
    exports :: Maybe (ExportSpecList l)
    exports = do
        ModuleHead _ _ _ maybeExports <- moduleHead
        maybeExports

    exportedModules :: [ModuleName ()]
    exportedModules = do
      ExportSpecList  _ specs   <- maybe [] one exports
      EModuleContents _ eModule <- specs
      pure $ dropAnn eModule

isInsideModule :: forall l. [Scoped l] -> ModuleName () -> Bool
isInsideModule annotations moduleName = any isNameUsed annotations
  where
    isNameUsed :: Scoped l -> Bool
    isNameUsed (scopedNameInfo -> nameInfo) = case nameInfo of
        GlobalSymbol _ (Qual _ usedName _) -> moduleName == usedName
        _                                  -> False
