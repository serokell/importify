{-# LANGUAGE ExplicitForAll      #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns        #-}

-- | Utilities to remove unused qualified imports

module Importify.Resolution.Qualified
       ( removeUnusedQualifiedImports
       ) where

import           Universum

import           Data.List                          (partition)

import           Language.Haskell.Exts              (ImportDecl (..), ModuleHead (..),
                                                     ModuleName (..), QName (..))
import           Language.Haskell.Names             (NameInfo (GlobalSymbol), Scoped)
import           Language.Haskell.Names.SyntaxUtils (dropAnn)

import           Importify.Syntax                   (isInsideExport, scopedNameInfo,
                                                     scopedNameInfo)

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

isInsideModule :: forall l. [Scoped l] -> ModuleName () -> Bool
isInsideModule annotations moduleName = any isNameUsed annotations
  where
    isNameUsed :: Scoped l -> Bool
    isNameUsed (scopedNameInfo -> nameInfo) = case nameInfo of
        GlobalSymbol _ (Qual _ usedName _) -> moduleName == usedName
        _                                  -> False
