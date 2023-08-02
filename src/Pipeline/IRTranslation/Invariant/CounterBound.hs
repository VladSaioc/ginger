module Pipeline.IRTranslation.Invariant.CounterBound (counterInvariants) where

import Backend.Ast
import Backend.Utilities
import Data.Map qualified as M
import Pipeline.IRTranslation.Utilities

-- Composes all counter invariants under conjunction
counterInvariants :: 𝛱 -> [Exp]
counterInvariants = map counterInvariant . M.toList

{- Constrict the value of pc(π) over viable program points.
Depends on: π, 𝜙

Produces:
0 <= pc(π) ∧ pc(π) <= (max ∘ dom)(𝜙)
-}
counterInvariant :: (P, 𝛷) -> Exp
counterInvariant (p, 𝜙) =
  let terminated = (𝜙 -|)
      pc = π p
      zero = (0 #)
      lower = zero :<= pc
      upper = pc :<= terminated
   in lower :&& upper