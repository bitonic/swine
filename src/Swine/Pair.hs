module Swine.Pair where

import           Prelude

data Pair a b = Pair
  { pairFst :: a
  , pairSnd :: b
  } deriving (Eq, Ord, Show, Read, Functor, Foldable, Traversable)
  -- I think these instances might be invalid because of strictness
