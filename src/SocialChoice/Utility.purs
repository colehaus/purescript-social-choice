module SocialChoice.Utility where

import Prelude

import Data.Foldable (class Foldable, foldl)
import Data.Maybe (Maybe(..))
import Data.Set (Set)
import Data.Set as Set

maximumsBy :: forall a f. Foldable f => Ord a => (a -> a -> Ordering) -> f a -> Set a
maximumsBy cmp = foldl max' Set.empty
  where
  max' s x =
    case Set.findMin s of
      Nothing -> Set.singleton x
      Just y ->
        case cmp x y of
          EQ -> Set.insert x s
          LT -> s
          GT -> Set.singleton x
