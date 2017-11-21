-- | This module contains additional utility functions for list.

module Extended.Data.List
       ( concatNub
       , foldMaybeMap
       , isIn
       , removeAt
       , removeAtMultiple
       ) where

import           Universum

-- | Removes element from list by given index. If there's
-- no element at such index then list returns unchanged.
removeAt :: Int -> [a] -> [a]
removeAt i l | i < 0 = l
removeAt i l = case after of
                     []       -> l
                     (_:rest) -> before ++ rest
  where (before, after) = splitAt i l

-- | Like 'removeAt' but takes list of indices to remove.
removeAtMultiple :: [Int] -> [a] -> [a]
removeAtMultiple indices = map snd
                         . filter ((`notElem` indices) . fst)
                         . zip [0..]

-- | Folds list of values with Maybe monoidal function.
foldMaybeMap :: Monoid m => (a -> Maybe m) -> [a] -> m
foldMaybeMap f = foldMap (maybeToMonoid . f)

-- | Flipped version of 'elem'.
isIn :: Eq a => [a] -> a -> Bool
isIn = flip elem
{-# INLINE isIn #-}

-- | Like 'concatMap' but also applies 'ordNub' to resulted list.
concatNub :: Ord b => (a -> [b]) -> [a] -> [b]
concatNub = ordNub ... concatMap
