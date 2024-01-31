-- | Functions that find the last matching element based on a predicate and then do something with the array.

module Data.Array.Extra.Last where

import Data.Array (findLastIndex, unsafeIndex, snoc, cons, take, drop, insertAt, unsnoc)
import Data.Array.Extra.Unsafe (unsafeDeleteAt, unsafeModifyAt, unsafeUpdateAt, unsafeInsertArray)
import Data.Eq (class Eq, (==))
import Data.Functor (map)
import Data.Maybe (Maybe(..))
import Data.Semiring ((+))
import Partial.Unsafe (unsafePartial)

-- | Find an element by a predicate and return it together with the array without the element.
-- |
-- | ```purescript
-- | pick (_ == 2) [1,2,3] == Just {yes: 2, no: [1,3]}
-- | ```
pick :: forall a. (a -> Boolean) -> Array a -> Maybe {yes :: a, no :: Array a}
pick f xs = case findLastIndex f xs of
  Nothing -> Nothing
  Just idx ->
    let elem = unsafePartial (unsafeIndex xs idx)
        rest = unsafePartial (unsafeDeleteAt idx xs)
    in Just {yes: elem, no: rest}

-- | Find an element and return an array without that element when it was found.
-- |
-- | ```purescript
-- | delete [2,1,3,2] 2 == Just [2,1,3]
-- | ```
delete :: forall a. Eq a => a -> Array a -> Maybe (Array a)
delete x xs = map (\idx -> unsafePartial (unsafeDeleteAt idx xs)) (findLastIndex (\i -> i == x) xs)

-- | Find an element by a predicate and return an array without that element when it was found.
-- |
-- | ```purescript
-- | deleteWith (_ == 2) [2,1,3,2] == Just [2,1,3]
-- | ```
deleteWith :: forall a. (a -> Boolean) -> Array a -> Maybe (Array a)
deleteWith f xs = map (\idx -> unsafePartial (unsafeDeleteAt idx xs)) (findLastIndex f xs)

-- | Find an element by a predicate and return an array with the updated element when it was found
-- |
-- | ```purescript
-- | updateWith (_ == 2) 4 [1,2,3] == Just [1,4,3]
-- | ```
updateWith :: forall a. (a -> Boolean) -> a -> Array a -> Maybe (Array a)
updateWith f a xs = map (\idx -> unsafePartial (unsafeUpdateAt idx a xs)) (findLastIndex f xs)

-- | Find an element by a predicate and return an array with the element replaced by an array.
-- |
-- | ```purescript
-- | updateFirstArrayWith (_ == 2) [21,22] [1,2,3,4,5] == Just [1,2,21,22,3,4,5]
-- | ```
updateFirstArrayWith :: forall a. Partial => (a -> Boolean) -> Array a -> Array a -> Maybe (Array a)
updateFirstArrayWith f xs ys = map (\idx -> unsafePartial (unsafeInsertArray idx xs ys)) (findLastIndex f ys)

-- | Find an element by a predicate and return an array with the updated element when it was found
-- |
-- | ```purescript
-- | modifyWith (_ == 2) (* 3) [1,2,3] == Just [1,6,3]
-- | ```
modifyWith :: forall a. (a -> Boolean) -> (a -> a) -> Array a -> Maybe (Array a)
modifyWith f modifier xs = map (\idx -> unsafePartial (unsafeModifyAt idx modifier xs)) (findLastIndex f xs)

-- | Modify an element when it was found by the predicate or append a new element to the end of the array.
-- |
-- | ```purescript
-- | modifyOrSnoc (_ == 2) (* 3) [1,2,3] 11 == Just [1,6,3]
-- | modifyOrSnoc (_ == 4) (* 3) [1,2,3] 11 == Just [1,2,3,11]
-- | ```
modifyOrSnoc :: forall a. (a -> Boolean) -> (a -> a) -> Array a -> a -> Array a
modifyOrSnoc f modifier xs x = case findLastIndex f xs of
  Nothing  -> snoc xs x
  Just idx -> unsafePartial (unsafeModifyAt idx modifier xs)

-- | Modify an element when it was found by the predicate or push a new element to the front of the array.
-- |
-- | ```purescript
-- | modifyOrCons (_ == 2) (* 3) 11 [1,2,3] == Just [1,6,3]
-- | modifyOrCons (_ == 4) (* 3) 11 [1,2,3] == Just [11,1,2,3]
-- | ```
modifyOrCons :: forall a. (a -> Boolean) -> (a -> a) -> a -> Array a -> Array a
modifyOrCons f modifier x xs = case findLastIndex f xs of
  Nothing  -> cons x xs
  Just idx -> unsafePartial (unsafeModifyAt idx modifier xs)

-- | Update an element when it was found by the predicate or append a new element to the end of the array.
-- |
-- | ```purescript
-- | updateOrSnoc (_ == 2) [1,2,3] 11 == Just [1,11,3]
-- | updateOrSnoc (_ == 4) [1,2,3] 11 == Just [1,2,3,11]
-- | ```
updateOrSnoc :: forall a. (a -> Boolean) -> Array a -> a -> Array a
updateOrSnoc f xs x = case findLastIndex f xs of
  Nothing  -> snoc xs x
  Just idx -> unsafePartial (unsafeUpdateAt idx x xs)

-- | Update an element when it was found by the predicate or push a new element to the front of the array.
-- |
-- | ```purescript
-- | updateOrCons (_ == 2) 11 [1,2,3] == Just [1,11,3]
-- | updateOrCons (_ == 4) 11 [1,2,3] == Just [11,1,2,3]
-- | ```
updateOrCons :: forall a. (a -> Boolean) -> a -> Array a -> Array a
updateOrCons f x xs = case findLastIndex f xs of
  Nothing  -> cons x xs
  Just idx -> unsafePartial (unsafeUpdateAt idx x xs)

-- | Finds a single element and returns it together with elements before and after.
-- |
-- | ```purescript
-- | partitionSides (_ == 3) [1,2,3,4,5] == Just {before: [1,2], found: 3, after: [4,5]}
-- | ```
splitOn :: forall a. (a -> Boolean) -> Array a -> Maybe {before :: Array a, found :: a, after :: Array a}
splitOn f xs = case findLastIndex f xs of
  Nothing  -> Nothing
  Just idx -> Just {before: take idx xs, found: unsafePartial (unsafeIndex xs idx), after: drop (idx + 1) xs}

-- | Find an element and place an element before it.
-- | Could also be thought of as placing the element in the place of the found element and moving al later elements.
-- |
-- | ```purescript
-- | insertBefore (_ == 2) 10 [1,2,3] == Just [1,10,2,3]
-- | ```
insertBefore :: forall a. (a -> Boolean) -> a -> Array a -> Maybe (Array a)
insertBefore f x xs = case findLastIndex f xs of
  Nothing  -> Nothing
  Just idx -> insertAt idx x xs

-- | Find an element and place an element after it.
-- |
-- | ```purescript
-- | insertAfter (_ == 2) 10 [1,2,3] == Just [1,2,10,3]
-- | ```
insertAfter :: forall a. (a -> Boolean) -> a -> Array a -> Maybe (Array a)
insertAfter f x xs = case findLastIndex f xs of
  Nothing  -> Nothing
  Just idx -> insertAt (idx + 1) x xs

-- | Find an element which could be projected into another value.
-- |
-- | ```purescript
-- | findMaybe (\x -> if x == 2 then Just "Found two" else Nothing) [1,2,3] == Just "Found Two"
-- | findMaybe (\x -> if x == 2 then Just 2 else Nothing) [1,2,3] == find (_ == 2) [1,2,3]
-- | ```
findMaybe :: forall a b. (a -> Maybe b) -> Array a -> Maybe b
findMaybe f xs = case unsnoc xs of
  Nothing             -> Nothing
  Just { init, last } -> case f last of
    Just projection -> Just projection
    Nothing         -> findMaybe f init

-- todo: difference