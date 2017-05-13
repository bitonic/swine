module Swine.Prelude
  ( module X
  , map
  , tshow
  , (>>)
  , (<$)
  ) where

import           Prelude as X (Bool(..), Either(..), return, id, error, Eq(..), (.), (<$>), Maybe(..), ($), length, (>), Int, (+), snd, show, Show, Applicative, FilePath, (-), fromIntegral, Ord, Read, toInteger, Integer, IO, mapM, (<*>), Monad, (=<<), fst, Char, String, concat, uncurry, words, readFile, fail)
import           Data.Text as X (Text)
import           Data.HashMap.Strict as X (HashMap)
import           Data.HashSet as X (HashSet)
import           Data.Semigroup as X ((<>))
import           Data.List as X (foldl', intersperse)
import           Data.Functor as X (Functor(fmap))
import           Data.Foldable as X (asum, Foldable, toList, foldMap)
import           Data.Traversable as X (Traversable, for)
import           Data.Int as X (Int64)
import           Data.String as X (IsString, fromString)
import           Data.Hashable as X (Hashable(..))
import           GHC.Generics as X (Generic)
import           Data.Monoid as X (mempty)
import           Control.Applicative as X (Alternative, (<*), pure)
import           Data.ByteString as X (ByteString)
import           Data.Void as X (Void, absurd)
import           Data.Functor.Identity as X (Identity(..), runIdentity)
import           Debug.Trace as X (trace, traceM)
import           Control.Monad as X (void, replicateM)
import           Control.Monad.IO.Class as X (liftIO)
import           Control.Monad.Reader as X (ReaderT, withReaderT)

import           Swine.LookupList as X (LookupList)
import           Swine.List as X
import           Swine.Option as X
import           Swine.Pair as X

map :: (Functor f) => (a -> b) -> f a -> f b
map = fmap

tshow :: (Show a, IsString s) => a -> s
tshow = fromString . show

(>>) :: (Applicative m) => m () -> m a -> m a
a >> b = (\_x y -> y) <$> a <*> b

(<$) :: (Applicative m) => a -> m () -> m a
x <$ m = m >> pure x
