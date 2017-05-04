-- Strict lists
module Swine.List where

import Prelude (Eq, Ord, Show, Read)
import Data.Foldable (Foldable)
import Data.Traversable (Traversable)
import Data.Monoid (Monoid(..))
import Data.Semigroup (Semigroup(..))
import Data.Functor (Functor)
import GHC.Generics (Generic)

data Fwd a = FwdNil | a :< Fwd a
  deriving (Eq, Ord, Show, Read, Generic, Functor, Foldable, Traversable)
instance Semigroup (Fwd a) where
  FwdNil <> ys = ys
  (x :< xs) <> ys = x :< (xs <> ys)
instance Monoid (Fwd a) where
  mempty = FwdNil
  mappend = (<>)

listToFwd :: [a] -> Fwd a
listToFwd = \case
  [] -> FwdNil
  x : xs -> x :< listToFwd xs

data Bwd a = BwdNil | Bwd a :> a
  deriving (Eq, Ord, Show, Read, Generic, Functor, Foldable, Traversable)
instance Semigroup (Bwd a) where
  xs <> BwdNil = xs
  xs <> (ys :> y) = (xs <> ys) :> y
instance Monoid (Bwd a) where
  mempty = BwdNil
  mappend = (<>)

bwdReverse :: Bwd a -> Fwd a
bwdReverse = go FwdNil
  where
    go prev = \case
      BwdNil -> FwdNil
      xs :> x -> go (x :< prev) xs
