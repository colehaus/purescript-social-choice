module SocialChoice.Ballot where

import Prelude

import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Map (Map)

-- TODO: HashMap

newtype Ballot opt val = MkBallot (Map opt val)
derive instance genericBallot :: Generic (Ballot opt val) _
derive instance eqBallot :: (Eq opt, Eq val) => Eq (Ballot opt val)
instance showBallot :: (Show opt, Show val) => Show (Ballot opt val) where
  show = genericShow
derive newtype instance semigroupBallot :: (Ord opt) => Semigroup (Ballot opt val)
derive newtype instance monoidBallot :: (Ord opt) => Monoid (Ballot opt val)

mk :: forall opt val. Map opt val -> Ballot opt val
mk = MkBallot
