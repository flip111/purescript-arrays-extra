module Data.Array.Extra.NonEmpty where

import Prelude

import Data.Array.Extra (partitionEithers)
import Data.Array.NonEmpty (NonEmptyArray)
import Data.Array.NonEmpty as NonEmptyArray
import Data.Either (Either)

-- | ```purescript
-- | partitionEithers1 (\a -> if a > 2 then Left a else Right a) [1,2,3,4] == Result: { head: Right 1, tail: { left: [3, 4], right: [2] } }
-- | ```
-- | other variant - is to return `Either { left NonEmptyArray l, right: Array r } { left Array l, right: NonEmptyArray r }`
partitionEithers1 :: forall a l r. (a -> Either l r) -> NonEmptyArray a -> { head :: Either l r, tail :: { left :: Array l, right :: Array r } }
partitionEithers1 f = NonEmptyArray.uncons >>> \{ head, tail } -> { head: f head, tail: partitionEithers f tail }
