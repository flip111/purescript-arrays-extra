-- | Some specialized functions can be found here.

module Data.Array.Extra where

import Data.Array (deleteBy, foldl, foldr, sortBy, intersectBy, filter, unionBy, uncons, snoc, null)
import Data.Either (Either(..))
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

-- | Find an element which could be projected into another value.
-- |
-- | ```purescript
-- | firstSuccessfulProjection (\x -> if x == 2 then Just "Found two" else Nothing) [1,2,3] == Just "Found Two"
-- | ```
firstSuccessfulProjection :: forall a b. (a -> Maybe b) -> Array a -> Maybe b
firstSuccessfulProjection f arr = case uncons arr of
  Nothing             -> Nothing
  Just { head, tail } -> case f head of
    Just projection -> Just projection
    Nothing -> firstSuccessfulProjection f tail

-- | Like unionBy between array A and array B. With elements left out from B being included when they match the predicate.
-- |
-- | Useful for updating matching elements and at the same time inserting new ones that match the insert criteria
-- |
-- | ```purescript
-- | unionByWhen
-- |   (\a b -> a.id == b.id)
-- |   (\{t} -> not $ null t)
-- |   [{id: 1, t: "old"}, {id: 2, t: "old"}]
-- |   [{id: 2, t: "new"}, {id: 3, t: ""}, {id: 4, t: "new"}]
-- |     = [{id: 1, t: "old"}, {id: 2, t: "new"}, {id: 4, t: "new"}]
-- | ```
-- |
-- | Truth table
-- |          | in A   | not in A    |
-- | in B     | update | insert-when |
-- | not in B | keep   | n/a         |
unionByWhen :: forall a. (a -> a -> Boolean) -> (a -> Boolean) -> Array a -> Array a -> Array a
unionByWhen eq f array_a array_b =
    let i = intersectBy eq array_a array_b
        n = filter f (differenceBy eq array_a array_b)
    in unionBy eq n i

-- | Map with a function that yields `Either`. Only succeeding when all elements where mapped to `Right`.
mapEither :: forall a b c. (a -> Either c b) -> Array a -> Either (Array c) (Array b)
mapEither f foldable =
  let {lefts, rights} = foldl (g f) {lefts: [], rights: []} foldable
  in  if null lefts then
          Right rights
        else
          Left lefts
  where g :: forall q r s. (q -> Either s r) -> {lefts :: Array s, rights :: Array r} -> q -> {lefts :: Array s, rights :: Array r}
        g f2 {lefts, rights} elem = case f2 elem of
          Left l -> {lefts: snoc lefts l, rights}
          Right r -> {lefts, rights: snoc rights r}
