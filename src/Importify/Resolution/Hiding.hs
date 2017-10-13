{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns    #-}

-- | Name resolvers for @hiding@ imports.

module Importify.Resolution.Hiding
       ( hidingUsedIn
       ) where

import           Universum

import           Language.Haskell.Exts  (Name)
import           Language.Haskell.Names (NameInfo (Export, GlobalSymbol), Scoped,
                                         Symbol (..))

import           Importify.Syntax       (anyAnnotation)

-- | Checks if given 'Symbol' is used in module annotations. This
-- function performs comparison by ignoring module names because we want
-- to remove @hiding@ by calling this function.
hidingUsedIn :: Symbol -> [Scoped l] -> Bool
hidingUsedIn symbol = anyAnnotation used
  where
    used :: NameInfo l -> Bool
    used (GlobalSymbol global _) = lossyCompare symbol global
    used (Export symbols)        = any (lossyCompare symbol) symbols
    used _                       = False

lossyCompare :: Symbol -> Symbol -> Bool
lossyCompare (Fun name1) (Fun name2) = name1 == name2
lossyCompare (Dat symb1) (Dat symb2) = modulelessEq symb1 symb2
lossyCompare _           _           = False

-- | Compares if two symbols are equal ignoring 'symbolModule'
-- field. Used to remove imports from @hiding@ sections.
modulelessEq :: Symbol -> Symbol -> Bool
modulelessEq this other = this { symbolModule = symbolModule other } == other

----------------------------------------------------------------------------
-- Patterns for conveninet comparison
----------------------------------------------------------------------------

funPattern :: Symbol -> Maybe (Name ())
funPattern Value{..}    = Just symbolName
funPattern Method{..}   = Just symbolName
funPattern Selector{..} = Just symbolName
funPattern _            = Nothing

datPattern :: Symbol -> Maybe Symbol
datPattern symb = case funPattern symb of
    Nothing -> Just symb
    Just _  -> Nothing

pattern Fun :: Name () -> Symbol
pattern Fun name <- (funPattern -> Just name)

pattern Dat :: Symbol -> Symbol
pattern Dat symb <- (datPattern -> Just symb)
