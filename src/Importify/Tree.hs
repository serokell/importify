module Importify.Tree
       ( removeIdentifiers
       , cleanQuals
       ) where

import           Universum

import           Data.Generics.Aliases (mkT)
import           Data.Generics.Schemes (everywhere)
import           Data.List             (notElem)
import           Language.Haskell.Exts (ImportDecl (..), ImportSpec (..),
                                        ImportSpecList (..), ModuleName (..), Name (..),
                                        Namespace (..), SrcSpanInfo)

import           Importify.Syntax      (Identifier, cnameToIdentifier, nameToIdentifier)

-- | Remove unused qualified renaming imports
cleanQuals :: [ModuleName SrcSpanInfo] -> [ImportDecl SrcSpanInfo] -> [ImportDecl SrcSpanInfo]
cleanQuals usedQuals = filter (qualModNeeded usedQuals)

qualModNeeded :: [ModuleName SrcSpanInfo] -> ImportDecl SrcSpanInfo -> Bool
qualModNeeded usedQuals ImportDecl{..} =
    case importAs of
        Just name -> isJust importSpecs ||
                     name `elem` usedQuals
        Nothing   -> True

-- | Remove a list of identifiers from ImportDecls.
removeIdentifiers :: [Identifier] -> [ImportDecl SrcSpanInfo] -> [ImportDecl SrcSpanInfo]
removeIdentifiers ids decls =
    (volatileImports ++) $
    cleanDecls $
    everywhere (mkT traverseToClean) $
    everywhere (mkT $ traverseToRemove ids) $
    everywhere (mkT $ traverseToRemoveThing ids)
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
    case newCnames of
        [] -> IAbs l (NoNamespace l) name
        _  -> IThingWith l name newCnames
  where
    newCnames = filter (\cname -> cnameToIdentifier cname `notElem` ids) cnames
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
idElem name = notElem (nameToIdentifier name)

-- | Traverses ImportDecls to remove empty import specs
traverseToClean :: ImportDecl SrcSpanInfo -> ImportDecl SrcSpanInfo
traverseToClean decl@ImportDecl{ importSpecs = Just (ImportSpecList _ _ []) } =
    decl { importSpecs = Nothing }
traverseToClean decl = decl

cleanDecls :: [ImportDecl SrcSpanInfo] -> [ImportDecl SrcSpanInfo]
cleanDecls = filter declNeeded
  where
    declNeeded :: ImportDecl SrcSpanInfo -> Bool
    declNeeded ImportDecl{importSpecs = Nothing} = False
    declNeeded _                                 = True
