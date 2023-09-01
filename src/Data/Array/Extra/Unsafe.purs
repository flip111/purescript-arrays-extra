-- | Unsafe but faster functions due to no out of bounds checking.

module Data.Array.Extra.Unsafe where

-- | Delete the element at index `i` in an array
-- |
-- | ```purescript
-- | unsafePartial $ unsafeDeleteAt 1 ["a", "b", "c"] = ["a", "c"]
-- | ```
unsafeDeleteAt :: forall a. Partial => Int -> Array a -> Array a
unsafeDeleteAt = unsafeDeleteAtImpl

foreign import unsafeDeleteAtImpl :: forall a. Int -> Array a -> Array a

-- | Insert the element at index `i` in an array
-- |
-- | ```purescript
-- | unsafePartial $ unsafeInsertAt 1 "b" ["a", "c"] = ["a", "b", "c"]
-- | ```
unsafeInsertAt :: forall a. Partial => Int -> a -> Array a -> Array a
unsafeInsertAt = unsafeInsertAtImpl

foreign import unsafeInsertAtImpl :: forall a. Int -> a -> Array a -> Array a

-- | Overwrite the element at index `i` in an array
-- |
-- | ```purescript
-- | unsafePartial $ unsafeUpdateAt 1 "e" ["a", "b", "c"] = ["a", "e", "c"]
-- | ```
unsafeUpdateAt :: forall a. Partial => Int -> a -> Array a -> Array a
unsafeUpdateAt = unsafeUpdateAtImpl

foreign import unsafeUpdateAtImpl :: forall a. Int -> a -> Array a -> Array a

-- | Overwrite the element at index `i` in an array
-- |
-- | ```purescript
-- | unsafePartial $ unsafeModifyAt 1 (\a -> a <> "e") ["a", "b", "c"] = ["a", "be", "c"]
-- | ```
unsafeModifyAt :: forall a. Partial => Int -> (a -> a) -> Array a -> Array a
unsafeModifyAt = unsafeModifyAtImpl

foreign import unsafeModifyAtImpl :: forall a. Int -> (a -> a) -> Array a -> Array a
