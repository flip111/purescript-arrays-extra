-- | Some specialized functions can be found here.

module Data.Array.Extra
  ( sortByMultiple
  , sortOn
  , sortOn'
  , sortOnBy
  , sortOnBy'
  , sortOnByMaybe
  , sortOnByMaybe'
  , partitionEithers
  , combinations
  , interleave
  , exactlyOne
  , maybeToArray
  , module Data.Foldable.Extra
  , module Data.Semigroup.Foldable.Extra
  , module Data.Traversable.Extra
  ) where

import Data.Foldable.Extra
import Data.Semigroup.Foldable.Extra
import Data.Traversable.Extra

import Control.Semigroupoid ((<<<))
import Data.Array (catMaybes, cons, head, length, sortBy, uncons, findIndex)
import Data.Array.NonEmpty (NonEmptyArray)
import Data.Array.NonEmpty as NEA
import Data.Either (Either)
import Data.Eq (class Eq, (==))
import Data.Filterable (partitionMap)
import Data.Foldable (fold)
import Data.Function (on)
import Data.Functor (map)
import Data.HeytingAlgebra ((||))
import Data.Map as Map
import Data.Maybe (Maybe(..), fromJust)
import Data.Ord (class Ord, comparing, (<), (>))
import Data.Ordering (Ordering(..))
import Data.Ring ((-))
import Data.Semigroup ((<>))
import Data.Tuple (Tuple(..), fst, snd)
import Partial.Unsafe (unsafePartial)

-- | Sort by multiple comparison functions.
-- | Similar to SQL where you can specificy multiple columns with ASC and DESC.
-- | When a comparison function gives EQ the next one is tried.
-- |
-- | Other functions don't have a Multiple variant such as sortOnByMultiple. Because the implementation of this function is so simple it serves as example function.
-- | `sortByMultiple comps xs = sortBy (fold comps) xs`
-- |
-- | ```purescript
-- | let datas = [{a: 2, b: 4}, {a: 3, b: 4}, {a: 2, b: 5}, {a: 1, b: 5}]
-- |     a {a: a1} {a: a2} = compare a1 a2
-- |     b {b: b1} {b: b2} = compare b1 b2 -- use flip for DESC, or alternatively: compare b2 b1
-- | in  sortByMultiple [a, flip b] datas = [{a: 1, b: 5}, {a: 2, b: 5}, {a: 2, b: 4}, {a: 3, b: 4}]
-- | ```
sortByMultiple :: forall a. Array (a -> a -> Ordering) -> Array a -> Array a
sortByMultiple comps xs = sortBy (fold comps) xs

-- | Sort a list by a projection.
-- |
-- | ```purescript
-- | sortOn (\x -> if x == "dog" then 2 else 1) ["apple", "dog", "kiwi"] = ["apple", "kiwi", "dog"]
-- | ```
sortOn :: forall a b. Ord b => (a -> b) -> Array a -> Array a
sortOn f = sortBy (comparing f)

-- | Sort a list by a projection.
-- | This version of sortOn uses the decorate-sort-undecorate paradigm or Schwartzian transform. Which means the projection for each entry will only be computed once at the cost of creating more data structures. You will have to benchmark your specific situation to find out whether `sortOn` or `sortOn'` is faster.
-- |
-- | ```purescript
-- | sortOn' (\x -> if x == "dog" then 2 else 1) ["apple", "dog", "kiwi"] = ["apple", "kiwi", "dog"]
-- | ```
sortOn' :: forall a b. Ord b => (a -> b) -> Array a -> Array a
sortOn' f = map snd <<< sortBy (comparing fst) <<< map (\x -> let y = f x in Tuple y x)

-- | Sort a list by a projection and sort by a given comparison function.
-- |
-- | ```purescript
-- | sortOnBy (\x -> if x == "dog" then 2 else 1) compare ["apple", "dog", "kiwi"] = ["apple", "kiwi", "dog"]
-- | ```
sortOnBy :: forall a b. (a -> b) -> (b -> b -> Ordering) -> Array a -> Array a
sortOnBy f comp = sortBy (comp `on` f)

-- | Sort a list by a projection and sort by a given comparison function.
-- | This version of sortOnBy uses the decorate-sort-undecorate paradigm or Schwartzian transform. Which means the projection for each entry will only be computed once at the cost of creating more data structures. You will have to benchmark your specific situation to find out whether `sortOnBy` or `sortOnBy'` is faster.
-- |
-- | ```purescript
-- | sortOnBy' (\x -> if x == "dog" then 2 else 1) compare ["apple", "dog", "kiwi"] = ["apple", "kiwi", "dog"]
-- | ```
sortOnBy' :: forall a b. (a -> b) -> (b -> b -> Ordering) -> Array a -> Array a
sortOnBy' f comp = map snd <<< sortBy (comp `on` fst) <<< map (\x -> let y = f x in Tuple y x)

-- | Sort a list by a projection and sort by a given comparison function.
-- | When the projection returns Nothing those items will be placed last. Useful if your Array contains items with missing data to sort on
-- |
-- | ```purescript
-- | sortOnByMaybe (\x -> if x == "dog" then Just 2 else Just 1) compare ["apple", "dog", "kiwi"] = ["apple", "kiwi", "dog"]
-- | ```
-- Implementers note: this could be simplified with https://github.com/purescript/purescript-prelude/issues/310
sortOnByMaybe :: forall a b. (a -> Maybe b) -> (b -> b -> Ordering) -> Array a -> Array a
sortOnByMaybe f comp xs =
  let
    g Nothing Nothing = EQ
    g (Just _) Nothing = GT
    g Nothing (Just _) = LT
    g (Just a) (Just b) = comp a b
  in
    sortOnBy f g xs

-- | Sort a list by a projection and sort by a given comparison function.
-- | When the projection returns Nothing those items will be placed last. Useful if your Array contains items with missing data to sort on
-- | This version of sortOnByMaybe uses the decorate-sort-undecorate paradigm or Schwartzian transform. Which means the projection for each entry will only be computed once at the cost of creating more data structures. You will have to benchmark your specific situation to find out whether `sortOnByMaybe` or `sortOnByMaybe'` is faster.
-- |
-- | ```purescript
-- | sortOnByMaybe' (\x -> if x == "dog" then Just 2 else Just 1) compare ["apple", "dog", "kiwi"] = ["apple", "kiwi", "dog"]
-- | ```
sortOnByMaybe' :: forall a b. (a -> Maybe b) -> (b -> b -> Ordering) -> Array a -> Array a
sortOnByMaybe' f comp xs =
  let
    g Nothing Nothing = EQ
    g (Just _) Nothing = GT
    g Nothing (Just _) = LT
    g (Just a) (Just b) = comp a b
  in
    sortOnBy' f g xs

-- | Partitions an array of Either into two arrays. All the Left elements are put, in order, into the left field of the output record. Similarly the Right elements are put into the right field of the output record.
-- | Note that this function is an alias for `partitionMap` from `Data.Filterable`.
-- | The function is included in this library for people who prefer this name for the function as they might be used to it from [haskell](https://hackage.haskell.org/package/base-4.19.0.0/docs/Data-Either.html#v:partitionEithers)
-- |
-- | ```purescript
-- | partitionEithers (\a -> if a > 2 then Left a else Right a) [1,2,3,4] == {left: [1,2], right: [3,4]}
-- | ```
partitionEithers :: forall a l r. (a -> Either l r) -> Array a -> { left :: Array l, right :: Array r }
partitionEithers = partitionMap

-- -- | Like unionBy between array A and array B. With elements left out from B being included when they match the predicate.
-- -- |
-- -- | Useful for updating matching elements and at the same time inserting new ones that match the insert criteria
-- -- |
-- -- | ```purescript
-- -- | unionByWhen
-- -- |   (\a b -> a.id == b.id)
-- -- |   (\{t} -> not $ null t)
-- -- |   [{id: 1, t: "old"}, {id: 2, t: "old"}]
-- -- |   [{id: 2, t: "new"}, {id: 3, t: ""}, {id: 4, t: "new"}]
-- -- |     = [{id: 1, t: "old"}, {id: 2, t: "new"}, {id: 4, t: "new"}]
-- -- | ```
-- -- |
-- -- | Truth table
-- -- |          | in A   | not in A    |
-- -- | in B     | update | insert-when |
-- -- | not in B | keep   | n/a         |
-- unionByWhen :: forall a. (a -> a -> Boolean) -> (a -> Boolean) -> Array a -> Array a -> Array a
-- unionByWhen eq f array_a array_b =
--     let i = intersectBy eq array_a array_b
--         n = filter f (differenceBy eq array_a array_b)
--     in unionBy eq n i

-- | Get all combinations when drawing n elements from an array.
-- | n must be greater than 0 and not greater than the length of the array otherwise this function returns `Nothing`
-- |
-- | ```purescript
-- | let products = [{product: "bread", amount: 1}, {product: "cheese", amount: 2}, {product: "bread", amount: 3}]
-- | combinations 2 [1,2,3] -> Just [[1,2], [2,3], [1,3]]
-- | ```
combinations :: forall a. Int -> Array a -> Maybe (Array (NonEmptyArray a))
combinations n xs =
  let
    f 0 _ = [ [] ]
    f nn xs' = case uncons xs' of
      Nothing -> []
      Just { head, tail } -> map (cons head) (f (nn - 1) tail) <> f n tail
  in
    if n < 1 || n > length xs then
      Nothing
    else
      Just (map (\x -> unsafePartial (fromJust (NEA.fromArray x))) (f n xs))

-- | Takes an element from each array in turn to produce a new array.
-- |
-- | ```purescript
-- | interleave [[1,2], [3,4]] = [1,3,2,4]
-- | concat     [[1,2], [3,4]] = [1,2,3,4]
-- | ```
interleave :: forall a. Array (Array a) -> Array a
interleave xss =
  let
    f :: Tuple (Array (Array a)) (Array a) -> Tuple (Array (Array a)) (Array a)
    f (Tuple [] acc) = Tuple [] acc
    f (Tuple xss' acc) =
      let
        heads_tails = catMaybes (map uncons xss')
      in
        f (Tuple (map (_.tail) heads_tails) (map (_.head) heads_tails <> acc))
  in
    snd (f (Tuple xss []))

-- | When the array has only one element, return this element.
-- | This function is the opposite from `singleton`.
-- |
-- | ```purescript
-- | exactlyOne [1] == Just 1
-- | exactlyOne [1,2] == Nothing
-- | ```
exactlyOne :: forall a. Array a -> Maybe a
exactlyOne xs = case length xs of
  1 -> head xs
  _ -> Nothing

-- | Convert a Maybe to an Array.
-- | This function is similar to `Data.Foldable.fromMaybe`. It named differently to avoid confusion with another `fromMaybe function`.
-- |
-- | * `Data.Maybe.fromMaybe :: forall a. a -> Maybe a -> a` Takes a default value
-- | * `Data.Unfoldable.fromMaybe :: forall f a. Unfoldable f => Maybe a -> f a` Does not take a default valut
-- |
-- | ```purescript
-- | maybeToArray Nothing == []
-- | maybeToArray (Just 2) == [2]
-- | ```
maybeToArray :: forall a. Maybe a -> Array a
maybeToArray Nothing = []
maybeToArray (Just value) = [ value ]

-- | Given two arrays, sort the second one by the order of the first one
-- | The elements of each array will be compared to each other with their own projection functions.
-- | Elements in the second array not found in the first one will be put at the end
-- |
-- | ```purescript
-- | orderByArray identity _.id [2,3,1] [{id: 1, name: "Sara"}, {id: 2, name: "John"}, {id: 3, name: "Miranda"}]
-- |   == [{id: 2, name: "John"}, {id: 3, name: "Miranda"}, {id: 1, name: "Sara"}]
-- | ```
orderByArray :: forall x y a. Eq a => (x -> a) -> (y -> a) -> Array x -> Array y -> Array y
orderByArray proj_x proj_y xs ys =
  let
    findIndexY y = findIndex (\x -> proj_x x == proj_y y) xs
    compareIndices y1 y2 =
      case Tuple (findIndexY y1) (findIndexY y2) of
        Tuple (Just i1) (Just i2) -> compare i1 i2
        Tuple (Just _) Nothing -> LT
        Tuple Nothing (Just _) -> GT
        Tuple Nothing Nothing -> EQ
  in
    sortBy compareIndices ys

-- | Given two arrays, sort the second one by the order of the first one
-- | The elements of each array will be compared to each other with their own projection functions.
-- | Elements in the second array not found in the first one will be put at the end
-- |
-- | Same as `orderByArray` but potentially faster due to using a Map internally. You will have to benchmark your specific situation to find out whether `orderByArray` or `orderByArrayOrd'` is faster.
orderByArrayOrd :: forall x y a. Ord a => (x -> a) -> (y -> a) -> Array x -> Array y -> Array y
orderByArrayOrd proj_x proj_y xs ys =
  let
    indexMap = Map.fromFoldable (mapWithIndex (\i x -> Tuple (proj_x x) i) xs)
  in
    sortBy (comparing (\y -> fromMaybe (length xs) $ Map.lookup (proj_y y) indexMap)) ys

-- zipOn :: forall a b c d. (a -> Maybe d) (b -> Maybe d) (a -> b -> c) -> Array a -> Array b -> Array c
-- zipOn f_a f_b c xs ys -- start searching the smallest array first
