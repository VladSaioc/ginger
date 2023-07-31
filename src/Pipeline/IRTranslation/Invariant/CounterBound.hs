module Pipeline.IRTranslation.Invariant.CounterBound (counterInvariants) where

import Backend.Ast
import Backend.Utilities
import Data.Map qualified as M
import Pipeline.IRTranslation.Utilities

-- Composes all counter invariants under conjunction
counterInvariants :: 𝛱 -> [Exp]
counterInvariants = map counterInvariant . M.toList

{- Constrict the value of pc(π) over viable program points.
Depends on: π, ϕ

Produces:
0 <= pc(π) ∧ pc(π) <= (max ∘ dom)(ϕ)
-}
counterInvariant :: (P, 𝛷) -> Exp
counterInvariant (pid, pp) =
  let terminated = (pp -|)
      pc = π pid
      zero = (0 #)
      lower = Leq zero pc
      upper = Leq pc terminated
   in lower :&& upper