-- | Some specialized functions can be found here.

module Data.Array.Extra where

import Control.Applicative (pure)
import Data.Array (deleteBy, foldl, foldr, sortBy, intersectBy, filter, unionBy, uncons, snoc, null, length, elem, all)
import Data.Array.Extra.First (modifyOrSnoc)
import Data.Either (Either(..))
import Data.Eq (class Eq, (==), (/=))
import Data.Function (on)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Ord (class Ord)
import Data.Ordering (Ordering)
import Data.Semigroup ((<>))
import Data.Semiring ((+))
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..))
import Data.Monoid (mempty)

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
-- | partitionMaybe (\x -> if x == "dog" then Just "cat" else Nothing) ["apple", "dog", "kiwi"] == { no: ["apple", "kiwi"], yes: ["cat"] }
-- | ```
partitionMaybe :: forall a b. (a -> Maybe b) -> Array a -> { no :: Array a, yes :: Array b }
partitionMaybe f xs = foldl go {yes: [], no: []} xs
  where go rec@{yes, no} x =
          case f x of
            Nothing -> rec {no  = no <> [x]}
            Just b  -> rec {yes = yes <> [b]}

-- | Find an element which could be projected into another value.
-- |
-- | ```purescript
-- | firstMaybe (\x -> if x == 2 then Just "Found two" else Nothing) [1,2,3] == Just "Found Two"
-- | ```
firstMaybe :: forall a b. (a -> Maybe b) -> Array a -> Maybe b
firstMaybe f arr = case uncons arr of
  Nothing             -> Nothing
  Just { head, tail } -> case f head of
    Just projection -> Just projection
    Nothing -> firstMaybe f tail

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
-- | Hint: if you don't care about collecting all the Left's (error conditions) and you are looking for a function like
-- | `forall a b c. (a -> Either c b) -> Array a -> Either c (Array b)` then use `traverse` from `Data.Traversable`.
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

-- | Count the amount of times a value occurs in an array.
-- | Requires an Ord instance for Map. This function should be faster than `occurrences`
-- |
-- | ```purescript
-- | occurrencesMap ["A", "B", "B"] == Map.fromList [Tuple "A" 1, Tuple "B" 2]
-- | ```
occurrencesMap :: forall a. Ord a => Array a -> Map a Int
occurrencesMap xs = foldl go Map.empty xs
  where go acc x = Map.insertWith (\old _ -> old + 1) x 1 acc

-- | Count the amount of times a value occurs in an array.
-- | Mostly useful for when you can not define an Ord instance
-- |
-- | ```purescript
-- | occurrences ["A", "B", "B"] == [Tuple "A" 1, Tuple "B" 2]
-- | ```
occurrences :: forall a. Eq a => Array a -> Array (Tuple a Int)
occurrences xs = foldl go [] xs
  where go acc x = modifyOrSnoc (\(Tuple k _) -> k == x) (\(Tuple k v) -> Tuple k (v + 1)) acc (Tuple x 1)

-- | Checks if two arrays have exactly the same elements.
-- | The order of elements does not matter.
-- |
-- | ```purescript
-- | sameElements ["A", "B", "B"] ["B", "A", "B"] == true
-- | sameElements ["A", "B", "B"] ["A", "B"] == false
-- | ```
sameElements :: forall a. Eq a => Array a -> Array a -> Boolean
sameElements a b = if length a /= length b then false else
  let occ_a = occurrences a
      occ_b = occurrences b
      go :: Tuple a Int -> Boolean
      go x = x `elem` occ_b
  in  all go occ_a

-- | Map an array conditionally, only return the array when at least one element was mapped.
-- | Elements that are not mapped will keep the old value.
-- |
-- | ```purescript
-- | mapAny (\_ -> Nothing) [1,2,3] == Nothing
-- | mapAny (\x -> if x == 2 then Just 99 else Nothing) [1,2,3] == Just [1,99,3]
-- | ```
mapAny :: forall a. (a -> Maybe a) -> Array a -> Maybe (Array a)
mapAny f xs =
  let go (Tuple acc replaced) x = case f x of
          Nothing -> Tuple (acc <> pure x) replaced
          Just y  -> Tuple (acc <> pure y) true

      Tuple acc replaced = foldl go (Tuple mempty false) xs

  in  if replaced then
        Just acc
      else
        Nothing

-- | Map an array conditionally, only return the array when all elements were mapped.
-- | Note that this function is an alias for `traverse`. This is specific behavior for the implementation of `<*>` for `Applicative Maybe`.
-- |
-- | ```purescript
-- | mapAll (\x -> if x == 2 then Just 99 else Nothing) [1,2,3] == Nothing
-- | mapAll (\x -> Just (x * 2)) [1,2,3] == Just [2,4,6]
-- | ```
mapAll :: forall a b. (a -> Maybe b) -> Array a -> Maybe (Array b)
mapAll = traverse
