module Swine.LookupList where

import           Prelude hiding (lookup)
import qualified Data.Set as S
import           Control.Monad (when)
import           Data.Monoid ((<>))

import           Swine.List
import           Swine.Pair

newtype LookupList a b = LookupList {toFwd :: Fwd (Pair a b)}
  deriving (Eq, Ord, Show, Read, Functor, Foldable, Traversable)
  -- I think these instances might be invalid because of strictness

empty :: LookupList a b
empty = LookupList mempty

null :: LookupList a b -> Bool
null (LookupList xs) = case xs of
  FwdNil -> True
  _ :< _ -> False

lookup :: (Eq a) => LookupList a b -> a -> Maybe b
lookup (LookupList ll) k = go ll
  where
    go = \case
      FwdNil -> Nothing
      Pair k' v :< kvs -> if k == k'
        then Just v
        else go kvs

fromList :: (Ord a) => [(a, b)] -> Either a (LookupList a b)
fromList xs0 = do
  xs <- check mempty xs0
  return (LookupList xs)
  where
    check seen = \case
      [] -> return FwdNil
      (k, v) : xs -> do
        when (S.member k seen) (Left k)
        (Pair k v :<) <$> check (S.insert k seen) xs

toList :: LookupList a b -> [(a, b)]
toList (LookupList xs) = go xs
  where
    go = \case
      FwdNil -> []
      Pair k v :< kvs -> (k, v) : go kvs

insert :: (Ord a) => LookupList a b -> a -> b -> LookupList a b
insert (LookupList ll) k v = LookupList (go BwdNil ll)
  where
    go prev = \case
      FwdNil -> bwdReverse (prev :> Pair k v)
      Pair k' v' :< kvs -> if k == k'
        then bwdReverse prev <> (Pair k v :< kvs)
        else go (prev :> Pair k' v') kvs
