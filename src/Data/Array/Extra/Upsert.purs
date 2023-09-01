module Data.Array.Extra.Upsert where

import Data.Array (cons, findIndex, snoc)
import Data.Array.Extra.Unsafe (unsafeModifyAt, unsafeUpdateAt)
import Data.Maybe (Maybe(..))
import Partial.Unsafe (unsafePartial)

-- | Modify an element when it was found by the predicate or append a new element to the end of the array.
-- |
-- | ```purescript
-- | modifyOrSnoc (== 2) (* 3) [1,2,3] 11 == Just [1,6,3]
-- | modifyOrSnoc (== 4) (* 3) [1,2,3] 11 == Just [1,2,3,11]
-- | ```
modifyOrSnoc :: forall a. (a -> Boolean) -> (a -> a) -> Array a -> a -> Array a
modifyOrSnoc pred modifier array x = case findIndex pred array of
  Nothing  -> snoc array x
  Just idx -> unsafePartial (unsafeModifyAt idx modifier array)

-- | Modify an element when it was found by the predicate or push a new element to the front of the array.
-- |
-- | ```purescript
-- | modifyOrCons (== 2) (* 3) 11 [1,2,3] == Just [1,6,3]
-- | modifyOrCons (== 4) (* 3) 11 [1,2,3] == Just [11,1,2,3]
-- | ```
modifyOrCons :: forall a. (a -> Boolean) -> (a -> a) -> a -> Array a -> Array a
modifyOrCons pred modifier x array = case findIndex pred array of
  Nothing  -> cons x array
  Just idx -> unsafePartial (unsafeModifyAt idx modifier array)

-- | Update an element when it was found by the predicate or append a new element to the end of the array.
-- |
-- | ```purescript
-- | updateOrSnoc (== 2) [1,2,3] 11 == Just [1,11,3]
-- | updateOrSnoc (== 4) [1,2,3] 11 == Just [1,2,3,11]
-- | ```
updateOrSnoc :: forall a. (a -> Boolean) -> a -> Array a -> a -> Array a
updateOrSnoc pred y array x = case findIndex pred array of
  Nothing  -> snoc array x
  Just idx -> unsafePartial (unsafeUpdateAt idx y array)

-- | Update an element when it was found by the predicate or push a new element to the front of the array.
-- |
-- | ```purescript
-- | updateOrCons (== 2) 11 [1,2,3] == Just [1,11,3]
-- | updateOrCons (== 4) 11 [1,2,3] == Just [11,1,2,3]
-- | ```
updateOrCons :: forall a. (a -> Boolean) -> a -> a -> Array a -> Array a
updateOrCons pred y x array = case findIndex pred array of
  Nothing  -> cons x array
  Just idx -> unsafePartial (unsafeUpdateAt idx y array)
