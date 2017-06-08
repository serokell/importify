
module Importify.Tree
       ( removeIdentifiers
       ) where

import           Universum

import           Data.Generics.Aliases (mkT)
import           Data.Generics.Schemes (everywhere)
import           Language.Haskell.Exts (ImportDecl (..), ImportSpec (..),
                                        ImportSpecList (..), Name (..), SrcSpanInfo)

import           Importify.Common      (Identifier, cnameToIdentifier, nameToIdentifier)

-- | Remove a list of identifiers from ImportDecls.
removeIdentifiers :: [Identifier] -> [ImportDecl SrcSpanInfo] -> [ImportDecl SrcSpanInfo]
removeIdentifiers ids decls =
    (volatileImports ++) $
    cleanDecls $
    (everywhere $ mkT $ traverseToClean) $
    (everywhere $ mkT $ traverseToRemove ids) $
    (everywhere $ mkT $ traverseToRemoveThing ids) $
    decls
  where
    volatileImports = filter isVolatileImport decls

-- | Returns True if the import is of either of the forms:
-- import Foo ()
-- import Foo
isVolatileImport :: ImportDecl SrcSpanInfo -> Bool
isVolatileImport ImportDecl{ importSpecs = Just (ImportSpecList _ _ []) } = True
isVolatileImport ImportDecl{ importSpecs = Nothing }                      = True
isVolatileImport _                                                        = False

-- | Traverses ImportDecls to remove identifiers from IThingWith specs
traverseToRemoveThing :: [Identifier] -> ImportSpec SrcSpanInfo -> ImportSpec SrcSpanInfo
traverseToRemoveThing ids (IThingWith l name cnames) =
    IThingWith l name (filter (\cname -> not $ elem (cnameToIdentifier cname) ids) cnames)
traverseToRemoveThing _ spec = spec

-- | Traverses ImportDecls to remove identifiers from ImportSpecs
traverseToRemove :: [Identifier] -> ImportSpecList SrcSpanInfo -> ImportSpecList SrcSpanInfo
traverseToRemove _ specs@(ImportSpecList _ True _) = specs -- Don't touch @hiding@ imports
traverseToRemove ids     (ImportSpecList l False specs) =
    ImportSpecList l False (filter (specNeeded ids) specs)

specNeeded :: [Identifier] -> ImportSpec SrcSpanInfo -> Bool
specNeeded ids (IVar _ name)          = idElem name ids
specNeeded ids (IAbs _ _ name)        = idElem name ids
specNeeded ids (IThingAll _ name)     = idElem name ids
specNeeded ids (IThingWith _ name []) = idElem name ids
specNeeded _   (IThingWith _ _ (_:_)) = True -- Do not remove if cnames list is not empty

idElem :: Name SrcSpanInfo -> [Identifier] -> Bool
idElem name = not . elem (nameToIdentifier name)

-- | Traverses ImportDecls to remove empty import specs
traverseToClean :: ImportDecl SrcSpanInfo -> ImportDecl SrcSpanInfo
traverseToClean decl@ImportDecl{ importSpecs = Just (ImportSpecList _ _ []) } =
    decl { importSpecs = Nothing }
traverseToClean decl = decl

cleanDecls :: [ImportDecl SrcSpanInfo] -> [ImportDecl SrcSpanInfo]
cleanDecls = filter declNeeded
  where
    declNeeded :: ImportDecl SrcSpanInfo -> Bool
    declNeeded (ImportDecl {importAs = Just _})     = True -- Don't touch @as@ imports for now
    declNeeded (ImportDecl {importSpecs = Nothing}) = False
    declNeeded _                                    = True
