{-# LANGUAGE ViewPatterns #-}

module Importify.Tree
       ( removeSymbols
       ) where

import           Universum

import           Data.Generics.Aliases  (mkT)
import           Data.Generics.Schemes  (everywhere)
import           Data.List              (notElem, partition)
import           Extended.Data.List     (removeAtMultiple)
import           Language.Haskell.Exts  (CName, ImportDecl (..), ImportSpec (..),
                                         ImportSpecList (..), Name (..), Namespace (..),
                                         SrcSpanInfo (..))
import           Language.Haskell.Names (NameInfo (..), Scoped (..))
import qualified Language.Haskell.Names as N

import           Importify.Syntax       (InScoped, pullScopedInfo)

-- | Remove a list of identifiers from 'ImportDecl's.
-- Next algorithm is used:
--
-- 1. Remove all identifiers inside specified list of types.
--    If list becomes empty then only type left.
-- @
--   import Module.Name (Type (HERE)) ⇒ import Module.Name (Type)
-- @
--
-- 2. Traverse every 'ImportSpec's and check matching with symbols.
-- @
--   import Module.Name (Type (something), HERE) ⇒ import Module.Name (Type (something))
-- @
--
-- 3. Translate empty imports into implicit import.
-- @
--    import Module.Name () ⇒ import Module.Name
-- @
--
-- 4. Remove all implicit imports preserving only initially implicit or empty.
--
removeSymbols :: [N.Symbol]            -- ^ List of symbols which should be removed
              -> [InScoped ImportDecl] -- ^ Imports to be purified
              -> [InScoped ImportDecl]
removeSymbols symbols decls =
    (volatileImports ++) $
    cleanDecls $
    everywhere (mkT traverseToClean) $
    everywhere (mkT $ traverseToRemove symbols) $
    everywhere (mkT $ traverseToRemoveThing symbols)
    decls
  where
    volatileImports = filter isVolatileImport decls

-- | Returns 'True' if the import is of either of the forms:
-- @
--   import Foo ()
--   import Foo
-- @
--
isVolatileImport :: InScoped ImportDecl -> Bool
isVolatileImport ImportDecl{ importSpecs = Just (ImportSpecList _ _ []) } = True
isVolatileImport ImportDecl{ importSpecs = Nothing }                      = True
isVolatileImport _                                                        = False

-- | Traverses 'ImportDecl's to remove symbols from 'IThingWith' specs.
traverseToRemoveThing :: [N.Symbol]
                      -> InScoped ImportSpec
                      -> InScoped ImportSpec
traverseToRemoveThing
    symbols
    (IThingWith (Scoped ni srcSpan@SrcSpanInfo{..}) name cnames)
  =
    case newCnames of
        [] -> IAbs (toScope emptySpan) (NoNamespace $ toScope emptySpan) name
        _  -> IThingWith (toScope newSpanInfo) name newCnames
  where
    emptySpan :: SrcSpanInfo
    emptySpan = SrcSpanInfo srcInfoSpan []

    toScope :: SrcSpanInfo -> Scoped SrcSpanInfo
    toScope = Scoped ni

    (newCnames, newSpanInfo) = removeSrcSpanInfoPoints isCNameNotInSymbols
                                                       cnames
                                                       srcSpan

    isCNameNotInSymbols :: InScoped CName -> Bool
    isCNameNotInSymbols (pullScopedInfo -> GlobalSymbol symbol _) = symbol `notElem` symbols
    isCNameNotInSymbols _                                         = False
traverseToRemoveThing _ spec = spec

-- | Traverses ImportDecls to remove identifiers from ImportSpecs
traverseToRemove :: [N.Symbol]
                 -> InScoped ImportSpecList
                 -> InScoped ImportSpecList
traverseToRemove _ specs@(ImportSpecList _ True _) = specs -- Don't touch @hiding@ imports
traverseToRemove symbols
                 (ImportSpecList (Scoped ni oldSpanInfo) notHiding specs)
  = let (neededSpecs, newSpanInfo) = removeSrcSpanInfoPoints (isSpecNeeded symbols)
                                                             specs
                                                             oldSpanInfo
    in ImportSpecList (Scoped ni newSpanInfo)
                      notHiding
                      neededSpecs

-- | Removes 'SrcSpanInfo' points of deleted elements.
removeSrcSpanInfoPoints :: (a -> Bool)  -- ^ Keep entitity?
                        -> [a]          -- ^ List of entities
                        -> SrcSpanInfo  -- ^ Span info with points
                        -> ([a], SrcSpanInfo) -- ^ Kept entities and info w/o points
removeSrcSpanInfoPoints shouldKeepEntity entities SrcSpanInfo{..} =
    let indexedEntities = zip [1..] entities
        (neededEntities, unusedEntities) = partition (shouldKeepEntity . snd)
                                                     indexedEntities
        pointsCount = length srcInfoPoints
        unusedIds   = filter (< pointsCount)  -- don't remove index of ')'
                    $ map fst unusedEntities
        newPoints   = removeAtMultiple unusedIds srcInfoPoints
    in (map snd neededEntities, SrcSpanInfo srcInfoSpan newPoints)

-- | Returns 'False' if 'ImportSpec' is not needed.
isSpecNeeded :: [N.Symbol] -> InScoped ImportSpec -> Bool
isSpecNeeded symbols (IVar _ name)          = isNameNeeded name symbols
isSpecNeeded symbols (IAbs _ _ name)        = isNameNeeded name symbols
isSpecNeeded symbols (IThingAll _ name)     = isNameNeeded name symbols
isSpecNeeded symbols (IThingWith _ name []) = isNameNeeded name symbols
isSpecNeeded _       (IThingWith _ _ (_:_)) = True -- Do not remove if cnames list is not empty

-- | Returns 'False' if 'Name' is not needed. On top level
-- elements inside 'ImportSpec' annotated by 'ImportPart'. But
-- this constructor contains list of symbols. So it's needed if
-- at least one element inside list is needed.
isNameNeeded :: InScoped Name -> [N.Symbol] -> Bool
isNameNeeded (pullScopedInfo -> ImportPart symbols) unusedSymbols =
    any (`notElem` unusedSymbols) symbols
isNameNeeded _  _                                    =
    True

-- | Traverses ImportDecls to remove empty import specs
traverseToClean :: InScoped ImportDecl -> InScoped ImportDecl
traverseToClean decl@ImportDecl{ importSpecs = Just (ImportSpecList _ _ []) } =
    decl { importSpecs = Nothing }
traverseToClean decl = decl

cleanDecls :: [InScoped ImportDecl] -> [InScoped ImportDecl]
cleanDecls = filter declNeeded
  where
    declNeeded :: InScoped ImportDecl -> Bool
    declNeeded = isJust . importSpecs
