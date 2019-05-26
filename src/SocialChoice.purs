module SocialChoice where

import Prelude
import SocialChoice.Utility

import Data.Either (fromRight)
import Data.Foldable (class Foldable)
import Data.Foldable as Foldable
import Data.Function (on)
import Data.HashMap as HashMap
import Data.HashSet (HashSet)
import Data.HashSet as HashSet
import Data.Hashable (class Hashable)
import Data.List as List
import Data.Map as Map
import Data.Maybe (fromJust)
import Data.Table (Table)
import Data.Table as Table
import Data.Tuple (Tuple(..), fst, snd)
import Data.Unfoldable1 as Unfold1
import Partial.Unsafe (unsafePartialBecause)

bordaChoice ::
  forall candidate voter cell.
  Ord cell => Hashable voter => Hashable candidate => Hashable cell =>
  Table candidate voter cell -> HashSet candidate
bordaChoice table =
  HashSet.map fst <<< maximumsBy (compare `on` snd) <<< HashMap.toArrayBy Tuple $
  candidateScores
  where
    candidateScores = Foldable.sum <<< HashMap.values <$> Table.rows' scoreTable
    second f (Tuple a b) = Tuple a (f b)
    scoreTable =
      unsafePartialBecause "bordaScore preserves length" $ fromRight $
      Table.mapColumns bordaScore table

bordaScore :: forall f cell. Foldable f => Functor f => Ord cell => f cell -> f Int
bordaScore cells =
  flip lookup' scores <$> cells
    where
      len = Foldable.length cells
      lookup' k m =
        unsafePartialBecause "Map formed from lookup values" $ fromJust $
        Map.lookup k m
      scores =
        Map.fromFoldable $
          List.zip
            (List.sort <<< List.fromFoldable $ cells)
            (Unfold1.range (len - 1) 0)
