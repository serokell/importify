-- | Resolvers for removing symbols from explicit import lists.

module Importify.Resolution.Explicit
       ( collectUnusedSymbolsBy
       , resolveModules
       , symbolUsedIn
       ) where

import           Universum

import           Data.Data                                (Data)
import qualified Data.Map.Strict                          as M

import           Language.Haskell.Exts                    (Module, ModuleName (..))
import           Language.Haskell.Names                   (NameInfo (Export, GlobalSymbol),
                                                           Scoped, resolve, symbolModule)
import qualified Language.Haskell.Names                   as N (Symbol (..), symbolName)
import           Language.Haskell.Names.GlobalSymbolTable (Table)

import           Importify.Syntax                         (anyAnnotation)


-- | Checks if 'Symbol' is used inside annotations. This function
-- needed to remove unused imports.
symbolUsedIn :: N.Symbol -> [Scoped l] -> Bool
symbolUsedIn symbol = anyAnnotation used
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

-- | Gather all symbols for given list of 'Module's.
resolveModules :: (Data l, Eq l) => [Module l] -> [(ModuleName (), [N.Symbol])]
resolveModules modules = M.toList $ resolve modules mempty
