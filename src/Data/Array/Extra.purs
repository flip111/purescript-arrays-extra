-- | Some specialized functions can be found here.

module Data.Array.Extra where

import Control.Applicative (pure)
import Data.Array (all, deleteBy, elem, filter, foldl, foldr, intersectBy, length, null, snoc, sortBy, unionBy)
import Data.Array.Extra.First (modifyOrSnoc)
import Data.Array.NonEmpty (NonEmptyArray)
import Data.Array.NonEmpty as NEA
import Data.Either (Either(..))
import Data.Eq (class Eq, (==), (/=))
import Data.Filterable (partitionMap)
import Data.Function (on)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Monoid (mempty)
import Data.Ord (class Ord)
import Data.Ordering (Ordering)
import Data.Semigroup ((<>))
import Data.Semiring ((+))
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..))

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
-- | mapMaybeAny (\_ -> Nothing) [1,2,3] == Nothing
-- | mapMaybeAny (\x -> if x == 2 then Just 99 else Nothing) [1,2,3] == Just [1,99,3]
-- | ```
mapMaybeAny :: forall a. (a -> Maybe a) -> Array a -> Maybe (Array a)
mapMaybeAny f xs =
  let go (Tuple acc replaced) x = case f x of
          Nothing -> Tuple (acc <> pure x) replaced
          Just y  -> Tuple (acc <> pure y) true

      Tuple acc replaced = foldl go (Tuple mempty false) xs

  in  if replaced then
        Just acc
      else
        Nothing

-- | Map an array conditionally, only return the array when all elements were mapped.
-- | Note that this function is an alias for `traverse` from `Data.Traversable`. This is specific behavior for the implementation of `<*>` for `Applicative Maybe`.
-- |
-- | ```purescript
-- | mapMaybeAll (\x -> if x == 2 then Just 99 else Nothing) [1,2,3] == Nothing
-- | mapMaybeAll (\x -> Just (x * 2)) [1,2,3] == Just [2,4,6]
-- | ```
mapMaybeAll :: forall a b. (a -> Maybe b) -> Array a -> Maybe (Array b)
mapMaybeAll = traverse

-- | Similar to group, adds the ability to group by a projection.
-- | The projection is returned as the first argument of the Tuple.
-- |
-- | ```purescript
-- | groupMaybe (\x -> Just $ if even x then "even" else "odd") [1,2,3] == [(Tuple "odd" [1,3]), (Tuple "even" [2])]
-- | ```
groupMaybe :: forall a b. Eq b => (a -> Maybe b) -> Array a -> Array (Tuple b (NonEmptyArray a))
groupMaybe f xs =
  let g :: Array (Tuple b (NonEmptyArray a)) -> a -> Array (Tuple b (NonEmptyArray a))
      g acc x = case f x of
        Nothing -> acc
        -- modifyOrSnoc :: forall a. (a -> Boolean) -> (a -> a) -> Array a -> a -> Array a
        Just v  -> modifyOrSnoc (\(Tuple acc_b _) -> acc_b == v) (\(Tuple acc_b nea) -> Tuple acc_b (NEA.snoc nea x)) acc (Tuple v (NEA.singleton x))
  in  foldl g [] xs

-- | Similar to groupMaybe, adds the ability to map over the thing being grouped.
-- | Useful for removing data that was only there to do the grouping.
-- |
-- | ```purescript
-- | groupMaybeMap (\x -> Just $ if even x then "even" else "odd") (*3) [1,2,3] == [(Tuple "odd" [3,9]), (Tuple "even" [6])]
-- | groupMaybeMap f identity xs = groupMaybe f xs
-- | ```
groupMaybeMap :: forall a b c. Eq b => (a -> Maybe b) -> (a -> c) -> Array a -> Array (Tuple b (NonEmptyArray c))
groupMaybeMap f g xs =
  let h :: Array (Tuple b (NonEmptyArray c)) -> a -> Array (Tuple b (NonEmptyArray c))
      h acc x = case f x of
        Nothing -> acc
        Just v  -> modifyOrSnoc (\(Tuple acc_b _) -> acc_b == v) (\(Tuple acc_b nea) -> Tuple acc_b (NEA.snoc nea (g x))) acc (Tuple v (NEA.singleton (g x)))
  in  foldl h [] xs

-- Partitions an array of Either into two arrays. All the Left elements are put, in order, into the left field of the output record. Similarly the Right elements are put into the right field of the output record.
-- Note that this function is an alias for `partitionMap` from `Data.Filterable`.
-- The function is included in this library for people who prefer this name for the function as they might be used to it from [haskell](https://hackage.haskell.org/package/base-4.19.0.0/docs/Data-Either.html#v:partitionEithers)
--
-- | ```purescript
-- | partitionEithers (\a -> if a > 2 then Left a else Right a) [1,2,3,4] == {left: [1,2], right: [3,4]}
-- | ```
partitionEithers :: forall a l r. (a -> Either l r) -> Array a -> {left :: Array l, right :: Array r}
partitionEithers = partitionMap
