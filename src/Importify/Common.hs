{-# LANGUAGE Rank2Types          #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Common utilities for import list processing

module Importify.Common
       ( Identifier (..)
       , collectImportsList
       , removeImportIdentifier
       ) where

import           Universum

import           Data.List             (delete)
import           Data.Map.Strict       (Map)
import qualified Data.Map.Strict       as M
import           Language.Haskell.Exts (ImportDecl (..), ImportSpec (..),
                                        ImportSpecList (..), Name (..))

newtype Identifier = Identifier { getIdentifier :: String }
    deriving (Show, Eq, Ord)

type ImportsListMap l = Map Identifier (ImportDecl l, ImportSpec l)

nameToIdentifier :: Name l -> Identifier
nameToIdentifier (Ident  _ name) = Identifier name
nameToIdentifier (Symbol _ name) = Identifier name

-- TODO: process all ctors of data type
importSpecToIdentifier :: ImportSpec l -> Identifier
importSpecToIdentifier (IVar _ name)              = nameToIdentifier name
importSpecToIdentifier (IAbs _ _ name)            = nameToIdentifier name
importSpecToIdentifier (IThingAll _ name)         = nameToIdentifier name
importSpecToIdentifier (IThingWith _ name _cname) = nameToIdentifier name


collectImportsList :: forall l . [ImportDecl l] -> ImportsListMap l
collectImportsList = foldr go mempty
  where
    go :: ImportDecl l -> ImportsListMap l -> ImportsListMap l
    go imp@ImportDecl{..} dict = maybe dict (updateWithImportSpecList dict imp) importSpecs

    updateWithImportSpecList :: ImportsListMap l
                             -> ImportDecl l
                             -> ImportSpecList l
                             -> ImportsListMap l
    updateWithImportSpecList dict _   (ImportSpecList _ True  _) = dict  -- True means is @hiding@
    updateWithImportSpecList dict imp (ImportSpecList _ False l) =
        foldr (\spec -> M.insert (importSpecToIdentifier spec) (imp, spec)) dict l

specListDelete :: Eq l => ImportSpec l -> ImportSpecList l -> ImportSpecList l
specListDelete spec (ImportSpecList l b specs) = ImportSpecList l b $ delete spec specs

deleteImportSpec :: Eq l => ImportSpec l -> ImportDecl l -> ImportDecl l
deleteImportSpec spec imp =
    maybe imp (\specs -> imp {importSpecs = Just $ specListDelete spec specs}) (importSpecs imp)

removeImportIdentifier :: Eq l
                       => Identifier
                       -> ImportsListMap l
                       -> [ImportDecl l]
                       -> ([ImportDecl l], ImportsListMap l)
removeImportIdentifier id dict decls = case M.lookup id dict of
    Nothing           -> (decls, dict)
    Just (decl, spec) -> ( replaceWithBy (== decl) (deleteImportSpec spec) decls
                         , M.delete id dict)

-- | Replace all entries in given list by applying given function
-- if predicate is @True@.
replaceWithBy :: (a -> Bool) -> (a -> a) -> [a] -> [a]
replaceWithBy p f = map (\a -> if p a then f a else a)
