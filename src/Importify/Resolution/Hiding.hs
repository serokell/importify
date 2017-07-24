-- | Name resolvers for @hiding@ imports.

module Importify.Resolution.Hiding
       ( hidingUsedIn
       ) where

import           Universum

import           Language.Haskell.Names (NameInfo (Export, GlobalSymbol), Scoped)
import qualified Language.Haskell.Names as N (Symbol (..))

import           Importify.Syntax       (anyAnnotation)

-- | Checks if given 'Symbol' is used in module annotations. This
-- function performs comparison by ignoring module names because we want
-- to remove @hiding@ by calling this function.
hidingUsedIn :: N.Symbol -> [Scoped l] -> Bool
hidingUsedIn symbol = anyAnnotation used
  where
    used :: NameInfo l -> Bool
    used (GlobalSymbol global _) = modulelessEq symbol global
    used (Export symbols)        = any (modulelessEq symbol) symbols
    used _                       = False

-- | Compares if two symbols are equal ignoring 'symbolModule'
-- field. Used to remove imports from @hiding@ sections.
modulelessEq :: N.Symbol -> N.Symbol -> Bool
modulelessEq this other = this { N.symbolModule = N.symbolModule other } == other
