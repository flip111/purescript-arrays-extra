-- | Functions that find the first matching element based on a predicate and then do something with the array.

module Data.Array.Extra.FindFirst where

import Data.Array (findIndex, unsafeIndex)
import Data.Array.Extra.Unsafe (unsafeDeleteAt, unsafeModifyAt, unsafeUpdateAt)
import Data.Functor (map)
import Data.Maybe (Maybe(..))
import Partial.Unsafe (unsafePartial)

-- | Find an element by a predicate and return it together with the array without the element.
-- |
-- | ```purescript
-- | partitionFirst (== 2) [1,2,3] == Just {yes: 2, no: [1,3]}
-- | ```
partitionFirst :: forall a. (a -> Boolean) -> Array a -> Maybe {yes :: a, no :: Array a}
partitionFirst predicate array = case findIndex predicate array of
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
deleteFirstWith :: forall a. (a -> Boolean) -> Array a -> Maybe (Array a)
deleteFirstWith pred array = map (\idx -> unsafePartial (unsafeDeleteAt idx array)) (findIndex pred array)

-- | Find an element by a predicate and return an array with the updated element when it was found
-- |
-- | ```purescript
-- | updateWith (== 2) 4 [1,2,3] == Just [1,4,3]
-- | ```
updateFirstWith :: forall a. (a -> Boolean) -> a -> Array a -> Maybe (Array a)
updateFirstWith pred a array = map (\idx -> unsafePartial (unsafeUpdateAt idx a array)) (findIndex pred array)

-- | Find an element by a predicate and return an array with the updated element when it was found
-- |
-- | ```purescript
-- | modifyWith (== 2) (* 3) [1,2,3] == Just [1,6,3]
-- | ```
modifyFirstWith :: forall a. (a -> Boolean) -> (a -> a) -> Array a -> Maybe (Array a)
modifyFirstWith pred modifier array = map (\idx -> unsafePartial (unsafeModifyAt idx modifier array)) (findIndex pred array)
