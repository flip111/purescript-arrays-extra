-- | Some specialized functions can be found here.

module Data.Array.Extra where

import Data.Array (deleteBy, foldl, foldr, sortBy)
import Data.Function (on)
import Data.Maybe (Maybe(..))
import Data.Ordering (Ordering)
import Data.Semigroup ((<>))

-- | Make a projection of an array then sort the original array by the projection
-- |
-- | ```purescript
-- | sortWithBy (\x -> if x == "dog" then 2 else 1) compare ["apple", "dog", "kiwi"] = ["apple", "kiwi", "dog"]
-- | ```
sortWithBy :: forall a b. (a -> b) -> (b -> b -> Ordering) -> Array a -> Array a
sortWithBy f comp = sortBy (comp `on` f)

-- | Like `difference` but takes a comparison function.
-- |
-- | ```purescript
-- | differenceBy (\a b -> toLower a == toLower b) ["apple", "dog"] ["KIWI", "DOG"] == ["KIWI"]
-- | ```
differenceBy :: forall a. (a -> a -> Boolean) -> Array a -> Array a -> Array a
differenceBy eq xs ys = foldr (deleteBy eq) xs ys

-- | Try to make a projection from an array. Return an array with the projection and the original array with the elements removed.
-- |
-- | ```purescript
-- | partitionProjection (\x -> if x == "dog" then Just "cat" else Nothing) ["apple", "dog", "kiwi"] == { no: ["apple", "kiwi"], yes: ["cat"] }
-- | ```
partitionProjection :: forall a b. (a -> Maybe b) -> Array a -> { no :: Array a, yes :: Array b }
partitionProjection f xs = foldl go {yes: [], no: []} xs
  where go rec@{yes, no} x =
          case f x of
            Nothing -> rec {no  = no <> [x]}
            Just b  -> rec {yes = yes <> [b]}
