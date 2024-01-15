-- | Functions that insert an element when it can not be found in the array.

module Data.Array.Extra.Insert where

import Data.Array (cons, elem, findIndex, insertBy, snoc, length)
import Data.Array.Extra.Unsafe (unsafeInsertArray)
import Data.Eq (class Eq)
import Data.HeytingAlgebra ((||))
import Data.Maybe (Maybe(..))
import Data.Ord ((<), (>=))
import Data.Ordering (Ordering)
import Partial.Unsafe (unsafePartial)

-- | Append an element to the end of the array when it could not be found by the predicate.
-- |
-- | ```purescript
-- | snocWith (_ == 2) [1,3] 2 == Just [1,3,2]
-- | ```
snocWith :: forall a. (a -> Boolean) -> Array a -> a -> Maybe (Array a)
snocWith pred xs x = case findIndex pred xs of
  Just _  -> Nothing
  Nothing -> Just (snoc xs x)
  
-- | Append an element to the end of the array when it could not be found.
-- |
-- | ```purescript
-- | snocWith' [1,3] 2 == Just [1,3,2]
-- | ```
snocWith' :: forall a. Eq a => Array a -> a -> Maybe (Array a)
snocWith' xs x = 
  if elem x xs then
    Nothing
  else
    Just (snoc xs x)

-- | Find an element by a predicate and when it was not found return an array with the element pushed to the front.
-- |
-- | ```purescript
-- | consWith 2 [1,3] == Just [2,1,3]
-- | ```
consWith :: forall a. (a -> Boolean) -> a -> Array a ->  Maybe (Array a)
consWith pred x xs = case findIndex pred xs of
  Just _  -> Nothing
  Nothing -> Just (cons x xs)

-- | Find an element and when it was not found return an array with the element pushed to the front.
-- |
-- | ```purescript
-- | consWith [1,3] 2 == Just [2,1,3]
-- | ```
consWith' :: forall a. Eq a => a -> Array a ->  Maybe (Array a)
consWith' x xs =
  if elem x xs then
    Nothing
  else
    Just (cons x xs)

-- | Insert the element into a sorted array but only if the element can not be found by the predicate.
-- |
-- | ```purescript
-- | insertByWith (_ == 2) (\a b -> compare a b) 2 [1,3]  == Just [1,2,3]
-- | ```
insertByWith :: forall a. (a -> Boolean) -> (a -> a -> Ordering) -> a -> Array a -> Maybe (Array a)
insertByWith pred comp x xs = case findIndex pred xs of
  Just _  -> Nothing
  Nothing -> Just (insertBy comp x xs)

-- | Insert the element into a sorted array but only if the element didn't exist before.
-- |
-- | ```purescript
-- | insertByWith (\a b -> compare a b) 2 [1,3]  == Just [1,2,3]
-- | ```
insertByWith' :: forall a. Eq a => (a -> a -> Ordering) -> a -> Array a -> Maybe (Array a)
insertByWith' comp x xs =
  if elem x xs then
    Nothing
  else
    Just (insertBy comp x xs)

-- | Insert an array into another array at the given position.
-- | Returns `Nothing` when the index is out of bounds.
-- |
-- | ```purescript
-- | unsafeInsertArray 2 [21,22] [1,2,3,4,5] == [1,2,21,22,3,4,5]
-- | ```
insertArray :: forall a. Int -> Array a -> Array a -> Maybe (Array a)
insertArray i xs ys = if i < 0 || i >= length ys then Nothing else Just (unsafePartial (unsafeInsertArray i xs ys))
