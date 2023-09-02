-- | This module defines functions that conditionally map over a functor.
-- | Outputting both the new functor and the old values that were replaced.

module Data.Array.Extra.FunctorWriter where

import Data.Maybe (Maybe, maybe)
import Data.Traversable (class Traversable, traverse)
import Data.Tuple (Tuple(..))

-- | Like map but extracts the replaced values into an array. Allows modification of the replaced values as well.
mapModify :: forall a f. Traversable f => (a -> {old :: a, new :: a}) -> f a -> Tuple (Array a) (f a)
mapModify f l = traverse (\a -> (\{old, new} -> Tuple [old] new) (f a)) l

-- | Conditionally map the function over the the functor. Returns a Tuple with the new functor and the replaced values.
mapMaybeWrite :: forall a f. Traversable f => (a -> Maybe a) -> f a -> Tuple (Array a) (f a)
mapMaybeWrite f l = traverse (\a -> maybe (Tuple [] a) (\a' -> Tuple [a] a') (f a)) l

-- | Same as mapMaybeWrite but allows modification of replaced values before returning them.
-- | Useful when the old and new values need to be linked to each other.
mapMaybeWriteModify :: forall a f. Traversable f => (a -> Maybe {old :: a, new :: a}) -> f a -> Tuple (Array a) (f a)
mapMaybeWriteModify f l = traverse (\a -> maybe (Tuple [] a) (\{old, new} -> Tuple [old] new) (f a)) l
