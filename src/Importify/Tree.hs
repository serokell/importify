{-# LANGUAGE ViewPatterns #-}

module Importify.Tree
       ( UnusedHidings (..)
       , UnusedSymbols (..)
       , removeImports
       ) where

import           Universum

import           Data.Generics.Aliases  (mkT)
import           Data.Generics.Schemes  (everywhere)
import           Data.List              (partition)
import           Extended.Data.List     (removeAtMultiple)
import           Language.Haskell.Exts  (CName, ImportDecl (..), ImportSpec (..),
                                         ImportSpecList (..), Name (..), Namespace (..),
                                         SrcSpanInfo (..))
import           Language.Haskell.Names (NameInfo (..), Scoped (..), Symbol)

import           Importify.Syntax       (InScoped, pullScopedInfo)

-- | @newtype@ wrapper for list of unused symbols.
newtype UnusedSymbols = UnusedSymbols { getUnusedSymbols :: [Symbol] }

-- | @newtype@ wrapper for list of unused symbols from @hiding@.
newtype UnusedHidings = UnusedHidings { getUnusedHidings :: [Symbol] }

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
removeImports :: UnusedSymbols        -- ^ List of symbols which should be removed
              -> UnusedHidings        -- ^ List of hidings which should be removed
              -> [InScoped ImportDecl] -- ^ Imports to be purified
              -> [InScoped ImportDecl]
removeImports (UnusedSymbols symbols) (UnusedHidings hidings) decls =
    (volatileImports ++)
  $ cleanDecls
  $ everywhere (mkT traverseToClean)
  $ everywhere (mkT $ traverseToRemove hidings True)
  $ everywhere (mkT $ traverseToRemove symbols False)
  $ everywhere (mkT $ traverseToRemoveThing symbols) decls
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
traverseToRemoveThing :: [Symbol]
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

-- | Traverses 'ImportSpecList' to remove identifiers from those lists.
traverseToRemove :: [Symbol]  -- ^
                 -> Bool
                 -> InScoped ImportSpecList
                 -> InScoped ImportSpecList
traverseToRemove symbols
                 yes'CleanIt
                 specList@(ImportSpecList (Scoped ni oldSpanInfo) isHiding specs)
    | isHiding == yes'CleanIt = newSpecs
    | otherwise               = specList
  where
    (neededSpecs, newSpanInfo) = removeSrcSpanInfoPoints (isSpecNeeded symbols)
                                                         specs
                                                         oldSpanInfo

    newSpecs = ImportSpecList (Scoped ni newSpanInfo)
                              isHiding
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
isSpecNeeded :: [Symbol] -> InScoped ImportSpec -> Bool
isSpecNeeded symbols (IVar _ name)          = isNameNeeded name symbols
isSpecNeeded symbols (IAbs _ _ name)        = isNameNeeded name symbols
isSpecNeeded symbols (IThingAll _ name)     = isNameNeeded name symbols
isSpecNeeded symbols (IThingWith _ name []) = isNameNeeded name symbols
isSpecNeeded _       (IThingWith _ _ (_:_)) = True -- Do not remove if cnames list is not empty

-- | Returns 'False' if 'Name' is not needed. On top level
-- elements inside 'ImportSpec' annotated by 'ImportPart'. But
-- this constructor contains list of symbols. So it's needed if
-- at least one element inside list is needed.
isNameNeeded :: InScoped Name -> [Symbol] -> Bool
isNameNeeded (pullScopedInfo -> ImportPart symbols) unusedSymbols =
    any (`notElem` unusedSymbols) symbols
isNameNeeded _ _ =
    True

-- | Traverses 'ImportDecl's to remove empty non-@hiding@ import specs.
traverseToClean :: InScoped ImportDecl -> InScoped ImportDecl
traverseToClean decl@ImportDecl{ importSpecs = Just (ImportSpecList _ False []) } =
    decl { importSpecs = Nothing }
traverseToClean decl = decl

-- | First remove all imports with no lists and then remove
-- 'ImportSpecList' from empty @hiding@ imports.
cleanDecls :: [InScoped ImportDecl] -> [InScoped ImportDecl]
cleanDecls = map removeHidingList . filter isDeclNeeded
  where
    removeHidingList :: InScoped ImportDecl -> InScoped ImportDecl
    removeHidingList decl@ImportDecl{ importSpecs = Just (ImportSpecList _ True []) } =
        decl { importSpecs = Nothing }
    removeHidingList decl = decl

    isDeclNeeded :: InScoped ImportDecl -> Bool
    isDeclNeeded = isJust . importSpecs
