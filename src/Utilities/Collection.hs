module Utilities.Collection where

import Data.Map qualified as M
import Data.Set qualified as S

-- An alias for the map type constructor to increase lisibility of type definitions
type a ↦ b = M.Map a b

class Collection a where
  zero :: a
  union :: a -> a -> a
  intersect :: a -> a -> a

(∅) :: Collection a => a
(∅) = zero

(∪) :: Collection a => a -> a -> a
(∪) = union

(∩) :: Collection a => a -> a -> a
(∩) = intersect

instance (Ord a, Collection b) => Collection (a ↦ b) where
  zero = M.empty
  union = M.unionWith (∪)
  intersect = M.intersectionWith (∩)

instance Ord a => Collection (S.Set a) where
  zero = S.empty
  union = S.union
  intersect = S.intersection

instance Eq a => Collection [a] where
  zero = []
  union = (++)
  intersect a1 a2 =
    case (a1, a2) of
      ([], _) -> []
      (_, []) -> []
      (a : a1', a' : a2') -> if a == a' then a : (a1' ∩ a2') else []
