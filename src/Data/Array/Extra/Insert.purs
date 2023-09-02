-- | Functions that insert an element when it can not be found in the array.

module Data.Array.Extra.Insert where

import Data.Array (cons, elem, findIndex, insertBy, snoc)
import Data.Eq (class Eq)
import Data.Maybe (Maybe(..))
import Data.Ordering (Ordering)

-- | Append an element to the end of the array when it could not be found by the predicate.
-- |
-- | ```purescript
-- | snocWith (== 2) [1,3] 2 == Just [1,3,2]
-- | ```
snocWith :: forall a. (a -> Boolean) -> Array a -> a ->  Maybe (Array a)
snocWith pred array x = case findIndex pred array of
  Just _  -> Nothing
  Nothing -> Just (snoc array x)
  
-- | Append an element to the end of the array when it could not be found by the predicate.
-- |
-- | ```purescript
-- | snocWith [1,3] 2 == Just [1,3,2]
-- | ```
snocWith' :: forall a. Eq a => Array a -> a -> Maybe (Array a)
snocWith' array x = 
  if elem x array then
    Nothing
  else
    Just (snoc array x)

-- | Find an element by a predicate and when it was not found return an array with the element pushed to the front.
-- |
-- | ```purescript
-- | consWith 2 [1,3] == Just [2,1,3]
-- | ```
consWith :: forall a. (a -> Boolean) -> a -> Array a ->  Maybe (Array a)
consWith pred x array = case findIndex pred array of
  Just _  -> Nothing
  Nothing -> Just (cons x array)

-- | Find an element by a predicate and when it was not found return an array with the element pushed to the front.
-- |
-- | ```purescript
-- | consWith [1,3] 2 == Just [2,1,3]
-- | ```
consWith' :: forall a. Eq a => a -> Array a ->  Maybe (Array a)
consWith' x array =
  if elem x array then
    Nothing
  else
    Just (cons x array)

-- | Insert the element into a sorted array but only if the element can not be found by the predicate.
-- |
-- | ```purescript
-- | insertByWith (== 2) (\a b -> compare a b) 2 [1,3]  == Just [1,2,3]
-- | ```
insertByWith :: forall a. (a -> Boolean) -> (a -> a -> Ordering) -> a -> Array a -> Maybe (Array a)
insertByWith pred comp x array = case findIndex pred array of
  Just _  -> Nothing
  Nothing -> Just (insertBy comp x array)

-- | Insert the element into a sorted array but only if the element didn't exist before.
-- |
-- | ```purescript
-- | insertByWith (\a b -> compare a b) 2 [1,3]  == Just [1,2,3]
-- | ```
insertByWith' :: forall a. Eq a => (a -> a -> Ordering) -> a -> Array a -> Maybe (Array a)
insertByWith' comp x array =
  if elem x array then
    Nothing
  else
    Just (insertBy comp x array)
