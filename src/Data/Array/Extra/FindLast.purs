-- | Functions that find the last matching element based on a predicate and then do something with the array.

module Data.Array.Extra.FindLast where

import Data.Array (findLastIndex, unsafeIndex)
import Data.Array.Extra.Unsafe (unsafeDeleteAt, unsafeModifyAt, unsafeUpdateAt)
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

-- | Find an element by a predicate and return an array with the updated element when it was found
-- |
-- | ```purescript
-- | modifyWith (== 2) (* 3) [1,2,3] == Just [1,6,3]
-- | ```
modifyLastWith :: forall a. (a -> Boolean) -> (a -> a) -> Array a -> Maybe (Array a)
modifyLastWith pred modifier array = map (\idx -> unsafePartial (unsafeModifyAt idx modifier array)) (findLastIndex pred array)
