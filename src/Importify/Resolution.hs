-- | This module contains functions to work with name resolution.

module Importify.Resolution
       ( -- * Symbols search engines
         collectUnusedSymbolsBy
       , collectUsedQuals

         -- * Predicates for unused imports
       , hidingUsedIn
       , symbolUsedIn

         -- * Utility resolvers
       , removeUnusedQualifiedAsImports
       , resolveModules
       ) where

import           Universum

import           Data.Data                                (Data)
import qualified Data.Map.Strict                          as M

import           Language.Haskell.Exts                    (ImportDecl (..), Module,
                                                           ModuleName (..), QName (..),
                                                           SrcSpanInfo)
import           Language.Haskell.Names                   (NameInfo (GlobalSymbol),
                                                           Scoped (Scoped), resolve)
import qualified Language.Haskell.Names                   as N
import           Language.Haskell.Names.GlobalSymbolTable (Table)
import           Language.Haskell.Names.SyntaxUtils       (getModuleName)

import           Importify.Syntax                         (scopedNameInfo)

-- This function returns list of symbols which names matches
-- given name. The result type is list because there're may be
-- several entities that share same name inside one module.
-- For example — type name and constructor name. Consider next example:
--
-- @
--   [ Constructor { symbolModule = ModuleName () "Language.Haskell.Exts.Syntax"
--                 , symbolName = Ident () "Module"
--                 , typeName = Ident () "Module" }
--   , Data { symbolModule = ModuleName () "Language.Haskell.Exts.Syntax"
--          , symbolName = Ident () "Module" }
--   ]
-- @
--
-- Currently we're not trying to guess 'Symbol' type by entity inside
-- 'ImportSpec'.
-- symbolsByName :: N.Symbol -> [N.Symbol] -> [N.Symbol]
-- symbolsByName name = filter (name ==)

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
        (N.symbolName symbol == stype && N.symbolModule symbol == smodule)

    -- ditto for selectors
    used (GlobalSymbol global@(N.Selector smodule _sname stype _scons) _) =
        symbol == global ||
        (N.symbolName symbol == stype && N.symbolModule symbol == smodule)

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
    used (GlobalSymbol global _) =
        symbol { N.symbolModule = N.symbolModule global } == global
    used _ = False

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

-- | Gather all symbols for given list of 'Module's. In reality those
-- modules represents all /exposed/ and /other/ modules for one package
-- returning only list of symbols for /exposed/ modules.
resolveModules :: (Data l, Eq l) => [Module l] -> [Module l] -> [(ModuleName (), [N.Symbol])]
resolveModules exposedModules otherModules =
    let symbolsEnv     = resolve (exposedModules ++ otherModules) mempty
        otherCleared   = map ((() <$) . getModuleName) otherModules

        -- remove @otherModules@ from environment because only @exposed@ can be imported
        exposedEnv     = foldl' (flip M.delete) symbolsEnv otherCleared
        exposedSymbols = M.assocs exposedEnv
    in exposedSymbols

-- | Remove unused @qualified as@ imports, i.e. in the next form:
-- @
--   import qualified Data.List as L
-- @
removeUnusedQualifiedAsImports :: [ImportDecl SrcSpanInfo]
                               -> [Scoped SrcSpanInfo]
                               -> [ImportDecl SrcSpanInfo]
removeUnusedQualifiedAsImports imports annotations =
    let usedQuals = collectUsedQuals imports annotations
    in filter (qualifiedAsImportNeeded usedQuals) imports

-- | Collect list of modules used for fully qualified names.
-- E.g. if it encounters "IO.putStrLn" it should collect @ModuleName "IO"@
-- Used later to determine whether @qualified@ import needed or not
collectUsedQuals :: [ImportDecl SrcSpanInfo] -> [Scoped SrcSpanInfo] -> [ModuleName SrcSpanInfo]
collectUsedQuals imports annotations = filter (\qual -> any (qualUsed qual) annotations) quals
  where
    quals :: [ModuleName SrcSpanInfo]
    quals = mapMaybe importAs $ filter (isNothing . importSpecs) imports

qualUsed :: ModuleName SrcSpanInfo -> Scoped SrcSpanInfo -> Bool
qualUsed (ModuleName _ name)
         (Scoped (GlobalSymbol _ (Qual _ (ModuleName _ usedName) _)) _)
  = name == usedName
qualUsed _ _ = False

qualifiedAsImportNeeded :: [ModuleName SrcSpanInfo]
                        -> ImportDecl SrcSpanInfo
                        -> Bool
qualifiedAsImportNeeded usedQuals ImportDecl{..} =
    case importAs of
        Just name -> isJust importSpecs
                  || name `elem` usedQuals
        Nothing   -> True
