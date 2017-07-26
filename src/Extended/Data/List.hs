-- | This module contains additional utility functions for list.

module Extended.Data.List
       ( mapMaybeM
       , removeAt
       , removeAtMultiple
       ) where

import           Universum

import           Data.List (notElem)

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

-- | A version of 'mapMaybe' that works with a monadic predicate.
--
-- TODO: replace with @wither@?
mapMaybeM :: Monad m => (a -> m (Maybe b)) -> [a] -> m [b]
mapMaybeM predicate = go
  where
    go []     = pure []
    go (a:as) = predicate a >>= \case
        Nothing -> go as
        Just b  -> fmap (b:) (go as)
