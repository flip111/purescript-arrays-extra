-- | This module defines functions that do something with an array based on a predicate.

module Data.Array.Extra.Find where

import Data.Array (cons, elem, findIndex, insertBy, snoc, unsafeIndex)
import Data.Array.Extra.Unsafe (unsafeDeleteAt, unsafeModifyAt, unsafeUpdateAt)
import Data.Eq (class Eq)
import Data.Functor (map)
import Data.Maybe (Maybe(..))
import Data.Ordering (Ordering)
import Partial.Unsafe (unsafePartial)

-- | Find an element by a predicate and return it together with the array without the element.
-- |
-- | ```purescript
-- | extract (== 2) [1,2,3] == Just {found: 2, remaining: [1,3]}
-- | ```
extract :: forall a. (a -> Boolean) -> Array a -> Maybe {found :: a, remaining :: Array a}
extract predicate array = case findIndex predicate array of
  Nothing -> Nothing
  Just idx ->
    let elem = unsafePartial (unsafeIndex array idx)
        rest = unsafePartial (unsafeDeleteAt idx array)
    in Just {found: elem, remaining: rest}

-- | Find an element by a predicate and return an array without that element when it was found.
-- |
-- | ```purescript
-- | deleteWith (== 2) [1,2,3] == Just [1,3]
-- | ```
deleteWith :: forall a. (a -> Boolean) -> Array a -> Maybe (Array a)
deleteWith pred array = map (\idx -> unsafePartial (unsafeDeleteAt idx array)) (findIndex pred array)

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
snocWith' :: forall a. Eq a => Array a -> a ->  Maybe (Array a)
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

-- | Find an element by a predicate and return an array with the updated element when it was found
-- |
-- | ```purescript
-- | updateWith (== 2) 4 [1,2,3] == Just [1,4,3]
-- | ```
updateWith :: forall a. (a -> Boolean) -> a -> Array a -> Maybe (Array a)
updateWith pred a array = map (\idx -> unsafePartial (unsafeUpdateAt idx a array)) (findIndex pred array)

-- | Find an element by a predicate and return an array with the updated element when it was found
-- |
-- | ```purescript
-- | modifyWith (== 2) (* 3) [1,2,3] == Just [1,6,3]
-- | ```
modifyWith :: forall a. (a -> Boolean) -> (a -> a) -> Array a -> Maybe (Array a)
modifyWith pred modifier array = map (\idx -> unsafePartial (unsafeModifyAt idx modifier array)) (findIndex pred array)
