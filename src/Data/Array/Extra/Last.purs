-- | Functions that find the last matching element based on a predicate and then do something with the array.

module Data.Array.Extra.Last where

import Data.Array (findLastIndex, unsafeIndex, snoc, cons)
import Data.Array.Extra.Unsafe (unsafeDeleteAt, unsafeModifyAt, unsafeUpdateAt, unsafeInsertArray)
import Data.Functor (map)
import Data.Maybe (Maybe(..))
import Partial.Unsafe (unsafePartial)

-- | Find an element by a predicate and return it together with the array without the element.
-- |
-- | ```purescript
-- | partitionLast (== 2) [1,2,3] == Just {yes: 2, no: [1,3]}
-- | ```
partitionLast :: forall a. (a -> Boolean) -> Array a -> Maybe {yes :: a, no :: Array a}
partitionLast predicate array = case findLastIndex predicate array of
  Nothing -> Nothing
  Just idx ->
    let elem = unsafePartial (unsafeIndex array idx)
        rest = unsafePartial (unsafeDeleteAt idx array)
    in Just {yes: elem, no: rest}

-- | Find an element by a predicate and return an array without that element when it was found.
-- |
-- | ```purescript
-- | deleteWith (== 2) [1,2,3] == Just [1,3]
-- | ```
deleteLastWith :: forall a. (a -> Boolean) -> Array a -> Maybe (Array a)
deleteLastWith pred array = map (\idx -> unsafePartial (unsafeDeleteAt idx array)) (findLastIndex pred array)

-- | Find an element by a predicate and return an array with the updated element when it was found
-- |
-- | ```purescript
-- | updateWith (== 2) 4 [1,2,3] == Just [1,4,3]
-- | ```
updateLastWith :: forall a. (a -> Boolean) -> a -> Array a -> Maybe (Array a)
updateLastWith pred a array = map (\idx -> unsafePartial (unsafeUpdateAt idx a array)) (findLastIndex pred array)

-- | Find an element by a predicate and return an array with the element replaced by an array.
-- |
-- | ```purescript
-- | updateFirstArrayWith (== 2) [21,22] [1,2,3,4,5] == Just [1,2,21,22,3,4,5]
-- | ```
updateFirstArrayWith :: forall a. Partial => (a -> Boolean) -> Array a -> Array a -> Maybe (Array a)
updateFirstArrayWith pred xs array = map (\idx -> unsafePartial (unsafeInsertArray idx xs array)) (findLastIndex pred array)

-- | Find an element by a predicate and return an array with the updated element when it was found
-- |
-- | ```purescript
-- | modifyWith (== 2) (* 3) [1,2,3] == Just [1,6,3]
-- | ```
modifyLastWith :: forall a. (a -> Boolean) -> (a -> a) -> Array a -> Maybe (Array a)
modifyLastWith pred modifier array = map (\idx -> unsafePartial (unsafeModifyAt idx modifier array)) (findLastIndex pred array)

-- | Modify an element when it was found by the predicate or append a new element to the end of the array.
-- |
-- | ```purescript
-- | modifyOrSnoc (== 2) (* 3) [1,2,3] 11 == Just [1,6,3]
-- | modifyOrSnoc (== 4) (* 3) [1,2,3] 11 == Just [1,2,3,11]
-- | ```
modifyOrSnoc :: forall a. (a -> Boolean) -> (a -> a) -> Array a -> a -> Array a
modifyOrSnoc pred modifier array x = case findLastIndex pred array of
  Nothing  -> snoc array x
  Just idx -> unsafePartial (unsafeModifyAt idx modifier array)

-- | Modify an element when it was found by the predicate or push a new element to the front of the array.
-- |
-- | ```purescript
-- | modifyOrCons (== 2) (* 3) 11 [1,2,3] == Just [1,6,3]
-- | modifyOrCons (== 4) (* 3) 11 [1,2,3] == Just [11,1,2,3]
-- | ```
modifyOrCons :: forall a. (a -> Boolean) -> (a -> a) -> a -> Array a -> Array a
modifyOrCons pred modifier x array = case findLastIndex pred array of
  Nothing  -> cons x array
  Just idx -> unsafePartial (unsafeModifyAt idx modifier array)

-- | Update an element when it was found by the predicate or append a new element to the end of the array.
-- |
-- | ```purescript
-- | updateOrSnoc (== 2) [1,2,3] 11 == Just [1,11,3]
-- | updateOrSnoc (== 4) [1,2,3] 11 == Just [1,2,3,11]
-- | ```
updateOrSnoc :: forall a. (a -> Boolean) -> Array a -> a -> Array a
updateOrSnoc pred array x = case findLastIndex pred array of
  Nothing  -> snoc array x
  Just idx -> unsafePartial (unsafeUpdateAt idx x array)

-- | Update an element when it was found by the predicate or push a new element to the front of the array.
-- |
-- | ```purescript
-- | updateOrCons (== 2) 11 [1,2,3] == Just [1,11,3]
-- | updateOrCons (== 4) 11 [1,2,3] == Just [11,1,2,3]
-- | ```
updateOrCons :: forall a. (a -> Boolean) -> a -> Array a -> Array a
updateOrCons pred x array = case findLastIndex pred array of
  Nothing  -> cons x array
  Just idx -> unsafePartial (unsafeUpdateAt idx x array)
