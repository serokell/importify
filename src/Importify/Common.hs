{-# LANGUAGE Rank2Types          #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Common utilities for import list processing

module Importify.Common
       ( Identifier (..)
       , collectImportsList
       , getImportModuleName
       , importSpecToIdentifier
       , importSlice
       , removeIdentifiers
       , removeImportIdentifier
       ) where

import           Universum

import           Data.List             (delete)
import qualified Data.List.NonEmpty    as NE
import           Data.Map.Strict       (Map)
import qualified Data.Map.Strict       as M

import           Language.Haskell.Exts (ImportDecl (..), ImportSpec (..),
                                        ImportSpecList (..), ModuleName, Name (..),
                                        SrcSpan (..), SrcSpanInfo (..), combSpanInfo)

-- | Returns module name for 'ImportDecl' with annotation erased.
getImportModuleName :: ImportDecl l -> ModuleName ()
getImportModuleName ImportDecl{..} = () <$ importModule

-- | Data type that represents function, operator, type or constructor identifier.
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


-- | Converts list of 'ImportDecl' to 'Map' from 'Identifier' @id@ to be able
-- to find easily corresponding 'ImportDecl' which export list contains @id@.
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

-- | Replace all entries in given list by applying given function
-- if predicate is @True@. After modification drop element from list if second
-- given predicate is also @True@. This function is basically some smart combination
-- of 'map' and 'filter'.
replaceOrDrop :: (a -> Bool) -> (a -> Bool) -> (a -> a) -> [a] -> [a]
replaceOrDrop shouldBeConsidered shouldBeDropped update = go
  where
    go [] = []
    go (x:xs) = if shouldBeConsidered x
                then let newX = update x in
                     if shouldBeDropped newX then xs else newX : xs
                else x : go xs

specListDelete :: Eq l => ImportSpec l -> ImportSpecList l -> Maybe (ImportSpecList l)
specListDelete spec (ImportSpecList l b specs) = case delete spec specs of
    [] -> Nothing
    sp -> Just $ ImportSpecList l b sp

deleteImportSpec :: Eq l => ImportSpec l -> ImportDecl l -> ImportDecl l
deleteImportSpec spec imp = imp {importSpecs = importSpecs imp >>= specListDelete spec }

-- | Find 'ImportDecl' that contains given entry identifier and remove this entry
-- from that import declaration.
removeImportIdentifier :: Eq l
                       => Identifier
                       -> ImportsListMap l
                       -> [ImportDecl l]
                       -> ([ImportDecl l], ImportsListMap l)
removeImportIdentifier id dict decls = case M.lookup id dict of
    Nothing           -> (decls, dict)
    Just (decl, spec) -> ( replaceOrDrop (eqByModule decl)
                                         (isNothing . importSpecs)
                                         (deleteImportSpec spec)
                                         decls
                         , M.delete id dict
                         )
  where
    eqByModule = (==) `on` getImportModuleName

-- | Removes list of 'Identifier's by calling 'removeImportIdentifier' and passing
-- new result to the next recursive call.
removeIdentifiers :: Eq l
                  => [Identifier]
                  -> ImportsListMap l
                  -> [ImportDecl l]
                  -> [ImportDecl l]
removeIdentifiers [] _ decls = decls
removeIdentifiers (id:ids) env decls =
    let (newDecls, newEnv) = removeImportIdentifier id env decls
    in removeIdentifiers ids newEnv newDecls

startAndEndLines :: SrcSpanInfo -> (Int, Int)
startAndEndLines (SrcSpanInfo SrcSpan{..} _) = (srcSpanStartLine, srcSpanEndLine)

-- | Returns pair of line numbers — first and last line of import section
-- if any import is in list.
importSlice :: [ImportDecl SrcSpanInfo] -> Maybe (Int, Int)
importSlice []               = Nothing
importSlice [ImportDecl{..}] = Just $ startAndEndLines importAnn
importSlice (x:y:xs)         = Just $ startAndEndLines
                                    $ combSpanInfo (importAnn x) (importAnn $ NE.last (y :| xs))
