module Swine.Prelude
  ( module X
  , map
  ) where

import           Prelude as X (Bool(..), Either(..), return, id, error, Eq(..), (.), (<$>), Maybe(..))
import           Data.Text as X (Text)
import           Data.HashMap.Strict as X (HashMap)
import           Data.Semigroup as X ((<>))
import           Data.List as X (foldl')
import           Data.Functor as X (fmap, Functor)
import           Data.Foldable as X (asum)

import           Swine.LookupList as X (LookupList)

map :: (Functor f) => (a -> b) -> f a -> f b
map = fmap
