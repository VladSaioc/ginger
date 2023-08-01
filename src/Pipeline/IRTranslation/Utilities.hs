module Pipeline.IRTranslation.Utilities where

import Backend.Ast
import Backend.Utilities
import Data.Map qualified as M

-- An alias for variable names (as strings) to clarify type definitions
type 𝑋 = String

-- An alias for the map type constructor to increase lisibility of type definitions
type a ↦ b = M.Map a b

-- The type of type environments
type 𝛴 = 𝑋 ↦ Type

-- An alias for the type of process ids. Its purpose is to provide
-- clarity to type definitions involving process ids.
type P = Int

-- An alias for the type of program points. Its purpose is to provide
-- clarity to type definitions involving program points.
type P𝑛 = Int

-- Bindings from program points to statements, encoding the semantics
-- of the operation at the given point.
type 𝛷 = P𝑛 ↦ Stmt

-- Bindings from process ids to program points.
type 𝛱 = P ↦ 𝛷

-- Program counter variable name. Produces the variable storing program
-- counters for each process. Given process id p,  the naming schema is
-- (contents between braces are swapped with the variable contents):
--  P{p}
(⊲) :: P -> String
(⊲) p = "P" ++ show p

-- Annotate process-local variable. Given process id p and name x,
-- the naming schema is (contents between braces are swapped with
-- the variable contents):
--  P{p}'{x}
(%) :: P -> 𝑋 -> 𝑋
(%) p x = (p ⊲) ++ "'" ++ x

-- Program id to program counter variable expression
π :: P -> Exp
π p = ((p ⊲) @)

-- Given a set of program points, produces the next available program point.
(-|) :: 𝛷 -> Exp
(-|) 𝜙 = case M.toDescList 𝜙 of
  [] -> (0 #)
  (𝑛, _) : _ -> (𝑛 #)

-- Checks that a sequence of values are all equal, by performing pair-wise structural equality.
equals :: Eq a => [a] -> Maybe a
equals = \case
  [] -> Nothing
  [a] -> Just a
  a' : a'' : as ->
    if a' == a'' then equals (a'' : as) else Nothing

-- Point-wise binary union between maps of maps of sets.
-- Given M1 and M2, it produces:
--
-- [ c ↦ [d ↦ M1(c)(d) ∪ M1(c)(d) | d ∈ dom(M1(c)) ∪ dom(M2(c))] | c ∈ dom(M1) ∪ dom(M2) ]
(⊎) :: Ord a => Ord b => a ↦ (b ↦ [c]) -> a ↦ (b ↦ [c]) -> a ↦ (b ↦ [c])
(⊎) = M.unionWith $ M.unionWith (++)
