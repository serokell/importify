-- | Haskell AST utilities to work with @import@ section.

module Importify.Syntax.Import
       ( getImportModuleName
       , importNamesWithTables
       , importSlice
       , importedSymbols
       , isImportImplicit
       , switchHidingImports
       ) where

import           Universum

import qualified Data.List.NonEmpty                       as NE
import           Language.Haskell.Exts                    (Extension (DisableExtension),
                                                           ImportDecl (..),
                                                           ImportSpecList (..),
                                                           KnownExtension (ImplicitPrelude),
                                                           Module (..), ModuleName,
                                                           ModuleName (..),
                                                           ModulePragma (LanguagePragma),
                                                           Name (Ident), SrcSpan (..),
                                                           SrcSpanInfo (..), combSpanInfo,
                                                           noSrcSpan, prettyExtension)
import           Language.Haskell.Names                   (NameInfo (Import, ImportPart),
                                                           Symbol)
import           Language.Haskell.Names.GlobalSymbolTable (Table)

import           Importify.Syntax.Module                  (isInsideExport)
import           Importify.Syntax.Scoped                  (InScoped, pullScopedInfo)

-- | Returns module name for 'ImportDecl' with annotation erased.
getImportModuleName :: ImportDecl l -> ModuleName ()
getImportModuleName ImportDecl{..} = () <$ importModule

-- | Returns 'True' iff import has next form:
-- @
--   import Module.Name
--   import Module.Name as Other.Name
--   import Module.Name hiding (smth)
-- @
isImportImplicit :: ImportDecl l -> Bool
isImportImplicit ImportDecl{ importQualified = True }                       = False
isImportImplicit ImportDecl{ importSpecs = Nothing }                        = True
isImportImplicit ImportDecl{ importSpecs = Just (ImportSpecList _ True _) } = True
isImportImplicit _                                                          = False

-- | This function returns name of import disregard to its
-- qualification, how this module should be referenced, i.e. like this:
-- @
--   import <whatever> A      ⇒ A
--   import <whatever> B as X ⇒ X
-- @
importReferenceName :: ImportDecl l -> ModuleName ()
importReferenceName ImportDecl{ importAs = Nothing, .. } = () <$ importModule
importReferenceName ImportDecl{ importAs = Just name   } = () <$ name

-- | Keep only hiding imports making them non-hiding. This function
-- needed to collect unused hiding imports because @importTable@ doesn't
-- track information about hiding imports.
switchHidingImports :: Module SrcSpanInfo -> Module SrcSpanInfo
switchHidingImports (Module ml mhead mpragmas mimports mdecls) =
    Module ml
           mhead
           ( LanguagePragma noSrcSpan
                            [ Ident noSrcSpan
                            $ prettyExtension
                            $ DisableExtension ImplicitPrelude
                            ]
           : mpragmas)
           (mapMaybe unhide mimports)
           mdecls
  where
    unhide :: ImportDecl SrcSpanInfo -> Maybe (ImportDecl SrcSpanInfo)
    unhide decl = do
        ImportSpecList l isHiding imports <- importSpecs decl
        guard isHiding
        guard $ not $ isInsideExport mhead (importReferenceName decl)
        pure $ decl { importSpecs = Just $ ImportSpecList l False imports }

switchHidingImports m = m

-- | Collect mapping from import name to to list of symbols it exports.
importNamesWithTables :: [InScoped ImportDecl] -> [(ModuleName (), Table)]
importNamesWithTables = map (getImportModuleName &&& getImportedSymbols)
  where
    getImportedSymbols :: InScoped ImportDecl -> Table
    getImportedSymbols = fromImportInfo . pullScopedInfo

    fromImportInfo :: NameInfo l -> Table
    fromImportInfo (Import dict) = dict
    fromImportInfo _             = mempty

-- | Extracts list of imported symbols from 'ImportPart'. Should be
-- called for implicit imports.
importedSymbols :: InScoped ImportDecl -> [Symbol]
importedSymbols ImportDecl{..} = do
    ImportPart symbols <- [pullScopedInfo importModule]
    symbols

-- | Returns pair of line numbers — first and last line of import section
-- if any import is in list.
importSlice :: [ImportDecl SrcSpanInfo] -> Maybe (Int, Int)
importSlice []               = Nothing
importSlice [ImportDecl{..}] = Just $ startAndEndLines importAnn
importSlice (x:y:xs)         = Just $ startAndEndLines
                                    $ combSpanInfo (importAnn x) (importAnn $ NE.last (y :| xs))

startAndEndLines :: SrcSpanInfo -> (Int, Int)
startAndEndLines (SrcSpanInfo SrcSpan{..} _) = (srcSpanStartLine, srcSpanEndLine)
