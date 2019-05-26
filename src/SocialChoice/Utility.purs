module SocialChoice.Utility where

import Prelude

import Data.Array as Array
import Data.Foldable (class Foldable, foldl)
import Data.HashSet (HashSet)
import Data.HashSet as HashSet
import Data.Hashable (class Hashable)
import Data.Maybe (Maybe(..))

maximumsBy :: forall a f. Foldable f => Hashable a => (a -> a -> Ordering) -> f a -> HashSet a
maximumsBy cmp = foldl max' HashSet.empty
  where
  max' s x =
    case Array.uncons <<< HashSet.toArray $ s of
      Nothing -> HashSet.singleton x
      Just y ->
        case cmp x y.head of
          EQ -> HashSet.insert x s
          LT -> s
          GT -> HashSet.singleton x
