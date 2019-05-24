module SocialChoice where

import Prelude

import Data.Either (fromRight)
import Data.Foldable (class Foldable)
import Data.Foldable as Foldable
import Data.Function (on)
import Data.List as List
import Data.Map as Map
import Data.Maybe (fromJust)
import Data.Set (Set)
import Data.Set as Set
import Data.Table (Table)
import Data.Table as Table
import Data.Tuple (Tuple(..), fst, snd)
import Data.Unfoldable1 as Unfold1
import Partial.Unsafe (unsafePartialBecause)

import SocialChoice.Utility

borda ::
  forall candidate voter cell.
  Ord cell => Ord voter => Ord candidate =>
  Table candidate voter cell -> Set candidate
borda table =
  Set.map fst $ maximumsBy (compare `on` snd) candidateScores
  where
    candidateScores = second (Foldable.sum <<< (snd <$> _)) <$> Table.rows' scoreTable
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
