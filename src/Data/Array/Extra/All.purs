module Data.Array.Extra.All where

import Data.Array (filter, length, range, zip, foldl)
import Data.Array.Extra.Unsafe (unsafeModifyAt, unsafeUpdateAt, unsafeInsertArray)
import Data.Functor (map)
import Data.Tuple (Tuple(..), snd)
import Partial.Unsafe (unsafePartial)

-- | Find the all the indices for which a predicate holds.
-- |
-- | ```purescript
-- | findIndices (contains $ Pattern "b") ["a", "bb", "b", "d"] = [1, 2]
-- | findIndices (contains $ Pattern "x") ["a", "bb", "b", "d"] = []
-- | ```
-- |
findIndices :: forall a. (a -> Boolean) -> Array a -> Array Int
findIndices f xs = map snd (filter (\(Tuple x _) -> f x) (zip xs (range 0 (length xs))))

-- | Find all elements matching a predicate and replace them with another element.
-- |
-- | ```purescript
-- | updateAllWith odd 4 [1,2,3] == Just [4,2,4]
-- | ```
updateAllWith :: forall a. (a -> Boolean) -> a -> Array a -> Array a
updateAllWith f a xs = foldl go xs (findIndices f xs)
  where go arr idx = unsafePartial (unsafeUpdateAt idx a arr)

-- | Find an element by a predicate and return an array with the element replaced by an array.
-- |
-- | ```purescript
-- | updateAllArrayWith odd [21,22] [1,2,3,4,5] == Just [21,22,2,21,22,4,21,22]
-- | ```
updateAllArrayWith :: forall a. Partial => (a -> Boolean) -> Array a -> Array a -> Array a
updateAllArrayWith f xs ys = foldl go ys (findIndices f ys)
  where go arr idx = unsafePartial (unsafeInsertArray idx xs arr)

-- | Find all elements matching a predicate and modify each element found.
-- |
-- | ```purescript
-- | modifyAllWith odd (* 3) [1,2,3] == Just [3,2,9]
-- | ```
modifyAllWith :: forall a. (a -> Boolean) -> (a -> a) -> Array a -> Array a
modifyAllWith f modifier xs = foldl go xs (findIndices f xs)
  where go arr idx = unsafePartial (unsafeModifyAt idx modifier arr)
