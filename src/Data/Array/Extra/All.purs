module Data.Array.Extra.All where

import Data.Array (filter, length, range, zip, foldl)
import Data.Functor (map)
import Data.Tuple (Tuple(..), snd)
import Partial.Unsafe (unsafePartial)
import Data.Array.Extra.Unsafe (unsafeModifyAt, unsafeUpdateAt, unsafeInsertArray)

-- | Find the all the indices for which a predicate holds.
-- |
-- | ```purescript
-- | findIndices (contains $ Pattern "b") ["a", "bb", "b", "d"] = [1, 2]
-- | findIndices (contains $ Pattern "x") ["a", "bb", "b", "d"] = []
-- | ```
-- |
findIndices :: forall a. (a -> Boolean) -> Array a -> Array Int
findIndices pred array = map snd (filter (\(Tuple x _) -> pred x) (zip array (range 0 (length array))))

-- | Find all elements matching a predicate and replace them with another element.
-- |
-- | ```purescript
-- | updateAllWith odd 4 [1,2,3] == Just [4,2,4]
-- | ```
updateAllWith :: forall a. (a -> Boolean) -> a -> Array a -> Array a
updateAllWith pred a array = foldl go array (findIndices pred array)
  where go arr idx = unsafePartial (unsafeUpdateAt idx a arr)

-- | Find an element by a predicate and return an array with the element replaced by an array.
-- |
-- | ```purescript
-- | updateAllArrayWith odd [21,22] [1,2,3,4,5] == Just [21,22,2,21,22,4,21,22]
-- | ```
updateAllArrayWith :: forall a. Partial => (a -> Boolean) -> Array a -> Array a -> Array a
updateAllArrayWith pred xs array = foldl go array (findIndices pred array)
  where go arr idx = unsafePartial (unsafeInsertArray idx xs arr)

-- | Find all elements matching a predicate and modify each element found.
-- |
-- | ```purescript
-- | modifyAllWith odd (* 3) [1,2,3] == Just [3,2,9]
-- | ```
modifyAllWith :: forall a. (a -> Boolean) -> (a -> a) -> Array a -> Array a
modifyAllWith pred modifier array = foldl go array (findIndices pred array)
  where go arr idx = unsafePartial (unsafeModifyAt idx modifier arr)
