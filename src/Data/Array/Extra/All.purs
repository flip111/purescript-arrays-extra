module Data.Array.Extra.All where

import Data.Array (filter, length, range, zip, foldl)
import Data.Array.Extra.Unsafe (unsafeModifyAt, unsafeUpdateAt, unsafeInsertArray, unsafeDeleteAt)
import Data.Eq (class Eq, (==))
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

-- todo: delete :: forall a. Eq a => a -> Array a -> Maybe (Array a)

-- | Find an element by a predicate and return an array without that element when it was found.
-- |
-- | ```purescript
-- | deleteWith (_ == 2) [2,1,3,2] == Just [1,3]
-- | ```
deleteWith :: forall a. (a -> Boolean) -> Array a -> Array a
deleteWith f xs = foldl (\ys idx -> unsafePartial (unsafeDeleteAt idx ys)) xs (findIndices f xs)

-- | Like `difference` in `Data.Array` but removes all of the elements from the first array which have a match in the second array.
-- |
-- | ```purescript
-- | difference [2, 1, 2] [2, 3] == [1,2] -- Data.Array
-- | difference [2, 1, 2] [2, 3] == [1]   -- Data.Array.Extra.All
-- | ```
difference :: forall a. Eq a => Array a -> Array a -> Array a
difference xs ys = foldl (\xss y -> deleteWith (\x -> x == y) xss) xs ys

-- todo: delete, differenceBy