module Pipeline.IRTranslation.Utilities where

import Backend.Ast qualified as T
import Backend.Utilities
import Data.Map qualified as M
import Utilities.Collection

-- | An alias for variable names (as strings) to clarify type definitions
type 𝑋 = String

-- | The type of type environments
type 𝛴 = 𝑋 ↦ T.Type

-- | An alias for the type of process ids. Its purpose is to provide
-- clarity to type definitions involving process ids.
type P = Int

-- | An alias for the type of program points. Its purpose is to provide
-- clarity to type definitions involving program points.
type 𝑁 = Int

-- | Bindings from program points to statements, encoding the semantics
-- of the operation at the given point.
type 𝛷 = 𝑁 ↦ T.Stmt

-- | Bindings from process ids to program points.
type 𝛱 = P ↦ 𝛷

-- | Bindings from process ids to program point reachability conditions.
type 𝛹 = P ↦ (𝑁 ↦ T.Exp)

-- | Program counter variable name. Produces the variable storing program
-- counters for each process. Given process id p, the naming schema is
-- (contents between braces are swapped with the variable contents):
--
-- > P{p}
(⊲) :: P -> String
(⊲) p = "P" ++ show p

-- | Program id to exit variable name. Produces the variable storing program
-- exit points for each process. Given process id p, the naming schema is
-- (contents between braces are swapped with the variable contents):
--
-- > X{p}
(▽) :: P -> String
(▽) p = "X" ++ show p

-- | Annotate process-local variable. Given process id p and name x,
-- the naming schema is (contents between braces are swapped with
-- the variable contents):
--
-- > P{p}'{x}
(%) :: P -> 𝑋 -> 𝑋
(%) p x = (p ⊲) ++ "'" ++ x

-- | Program id to program counter variable expression.
π :: P -> T.Exp
π p = ((p ⊲) @)

-- | Program id to exit variable expression.
𝜒 :: P -> T.Exp
𝜒 p = ((p ▽) @)

-- | Given a set of program points, produces the next available program point.
(-|) :: 𝛷 -> T.Exp
(-|) 𝜙 = case M.toDescList 𝜙 of
  [] -> (0 #)
  (𝑛, _) : _ -> (𝑛 #)

-- | Checks that a sequence of values are all equal, by performing pair-wise structural equality.
equals :: Eq a => [a] -> Maybe a
equals = \case
  [] -> Nothing
  [a] -> Just a
  a' : a'' : as ->
    if a' == a'' then equals (a'' : as) else Nothing
