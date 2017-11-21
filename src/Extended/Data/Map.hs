{-# LANGUAGE ExplicitForAll      #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}

module Extended.Data.Map
       ( groupBy
       , insertNonEmpty
       , invertMapToList
       , mapBy
       ) where

import Universum

import qualified Data.Map as Map
import qualified Data.Semigroup as Semigroup ((<>))

-- | Inserts one element to map from keys to 'NonEmpty' list.
insertNonEmpty :: Ord k => k -> v -> Map k (NonEmpty v) -> Map k (NonEmpty v)
insertNonEmpty k v = Map.insertWith (flip (Semigroup.<>)) k (one v)

-- | Groups 'NonTrivialContainer' into 'Map' by given function.
groupBy :: forall f a b .
           (NontrivialContainer f, Element f ~ a, Eq b, Ord b)
        => (a -> b)
        -> f
        -> Map b (NonEmpty a)
groupBy f = foldl' insertBy mempty
  where
    insertBy :: Map b (NonEmpty a) -> a -> Map b (NonEmpty a)
    insertBy dict value = insertNonEmpty (f value) value dict

-- | Like 'groupBy' but keeps only one element of mapping.
mapBy :: forall f a b .
         (NontrivialContainer f, Element f ~ a, Eq b, Ord b)
      => (a -> b)
      -> f
      -> Map b a
mapBy f = foldl' insertBy mempty
  where
    insertBy :: Map b a -> a -> Map b a
    insertBy dict value = Map.insert (f value) value dict

-- | Creates Map from values to keys by mapping each value to corresponding key.
invertMapToList :: forall k v . (Ord k, Ord v) => Map k [v] -> Map v k
invertMapToList = foldl' insertList mempty . Map.toList
  where
    insertList :: Map v k -> (k, [v]) -> Map v k
    insertList valueMap (key, values) =
        foldl' (\vals val -> Map.insert val key vals) valueMap values
