module Swine.LookupList where

import           Prelude hiding (lookup)
import qualified Prelude

newtype LookupList a b = LookupList [(a, b)]

lookup :: (Eq a) => LookupList a b -> a -> Maybe b
lookup (LookupList ll) x = Prelude.lookup x ll
