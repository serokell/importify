-- | Resolvers to remove implicit imports.

module Importify.Resolution.Implicit
       ( collectUnusedImplicitImports
       , isKnownImport
       , removeImplicitImports
       ) where

import           Universum

import           Data.List                     (notElem)
import qualified Data.Map.Strict               as M

import           Language.Haskell.Exts         (ImportDecl (..), ModuleName (..))
import           Language.Haskell.Names        (Environment)
import qualified Language.Haskell.Names        as N (Symbol (..))

import           Importify.Resolution.Explicit (collectUnusedSymbolsBy)
import           Importify.Syntax              (InScoped, getImportModuleName,
                                                importNamesWithTables, isImportImplicit)

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
    notImplicitOrUnused imp = not (isImportImplicit imp)
                           || getImportModuleName imp `notElem` names
