-- | Haskell AST utilities to work with @import@ section.

module Importify.Syntax.Import
       ( getImportModuleName
       , importNamesWithTables
       , importSlice
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
import           Language.Haskell.Names                   (NameInfo (Import))
import           Language.Haskell.Names.GlobalSymbolTable (Table)

import           Importify.Syntax.Scoped                  (InScoped, pullScopedInfo)

-- | Returns module name for 'ImportDecl' with annotation erased.
getImportModuleName :: ImportDecl l -> ModuleName ()
getImportModuleName ImportDecl{..} = () <$ importModule

-- | Returns 'True' iff import has next form:
-- @
--   import Module.Name
-- @
isImportImplicit :: ImportDecl l -> Bool
isImportImplicit ImportDecl{ importQualified = True }                       = False
isImportImplicit ImportDecl{ importSpecs = Nothing }                        = True
isImportImplicit ImportDecl{ importSpecs = Just (ImportSpecList _ True _) } = True
isImportImplicit _                                                          = False

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
    unhide decl@ImportDecl{..} = importSpecs >>= \case
        ImportSpecList _ False _      -> Nothing
        ImportSpecList l True imports -> Just
                                       $ decl {importSpecs = Just
                                                           $ ImportSpecList l False imports
                                              }
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

-- | Returns pair of line numbers — first and last line of import section
-- if any import is in list.
importSlice :: [ImportDecl SrcSpanInfo] -> Maybe (Int, Int)
importSlice []               = Nothing
importSlice [ImportDecl{..}] = Just $ startAndEndLines importAnn
importSlice (x:y:xs)         = Just $ startAndEndLines
                                    $ combSpanInfo (importAnn x) (importAnn $ NE.last (y :| xs))

startAndEndLines :: SrcSpanInfo -> (Int, Int)
startAndEndLines (SrcSpanInfo SrcSpan{..} _) = (srcSpanStartLine, srcSpanEndLine)
