-- | Utility functions to work with 'Scoped' data type from
-- @haskell-names@ library.

module Importify.Syntax.Scoped
       ( InScoped
       , anyAnnotation
       , pullScopedInfo
       , scopedNameInfo
       , unscope
       ) where

import           Universum

import           Language.Haskell.Exts  (Annotated (ann), SrcSpanInfo (..))
import           Language.Haskell.Names (NameInfo, Scoped (..))

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

anyAnnotation :: (NameInfo l -> Bool) -> [Scoped l] -> Bool
anyAnnotation used = any used . map scopedNameInfo
