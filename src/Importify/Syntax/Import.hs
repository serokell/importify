-- | Haskell AST utilities to work with @import@ section.

module Importify.Syntax.Import
       ( getImportModuleName
       , importSlice
       , switchHidingImports
       ) where

import           Universum

import qualified Data.List.NonEmpty                 as NE
import qualified Data.Text                          as T
import           Language.Haskell.Exts              (Annotated (ann),
                                                     Extension (DisableExtension),
                                                     ImportDecl (..), ImportSpecList (..),
                                                     KnownExtension (ImplicitPrelude),
                                                     Module (..), ModuleName,
                                                     ModuleName (..),
                                                     ModulePragma (LanguagePragma),
                                                     Name (Ident), SrcSpan (..),
                                                     SrcSpanInfo (..), combSpanInfo,
                                                     noSrcSpan, prettyExtension)
import           Language.Haskell.Names             (NameInfo, Scoped (..))
import           Language.Haskell.Names.SyntaxUtils (getModuleName)
import           Text.Show.Pretty                   (ppShow)

-- | Returns module name for 'ImportDecl' with annotation erased.
getImportModuleName :: ImportDecl l -> ModuleName ()
getImportModuleName ImportDecl{..} = () <$ importModule

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

startAndEndLines :: SrcSpanInfo -> (Int, Int)
startAndEndLines (SrcSpanInfo SrcSpan{..} _) = (srcSpanStartLine, srcSpanEndLine)

-- | Returns pair of line numbers — first and last line of import section
-- if any import is in list.
importSlice :: [ImportDecl SrcSpanInfo] -> Maybe (Int, Int)
importSlice []               = Nothing
importSlice [ImportDecl{..}] = Just $ startAndEndLines importAnn
importSlice (x:y:xs)         = Just $ startAndEndLines
                                    $ combSpanInfo (importAnn x) (importAnn $ NE.last (y :| xs))
