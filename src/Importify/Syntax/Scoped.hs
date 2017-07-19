-- | Utility functions to work with 'Scoped' data type from
-- @haskell-names@ library.

module Importify.Syntax.Scoped
       ( InScoped
       , pullScopedInfo
       , scopedNameInfo
       , unscope
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

-- | Short wrapper for types annotated by @Scoped SrcSpanInfo@.
-- For example, use @InScoped ImportDecl@ instead of @ImportDecl (Scoped SrcSpanInfo)@.
type InScoped t = t (Scoped SrcSpanInfo)

-- | Retrive 'NameInfo' from 'Scoped'.
scopedNameInfo :: Scoped l -> NameInfo l
scopedNameInfo (Scoped info _) = info

-- | Retrive 'NameInfo' from something annotated by 'Scoped'.
pullScopedInfo :: Annotated ast => ast (Scoped l) -> NameInfo l
pullScopedInfo = scopedNameInfo . ann

-- | Drop 'Scoped' annotation from 'Functor' type.
unscope :: Functor f => f (Scoped l) -> f l
unscope = fmap $ \case Scoped _ l -> l
