module Pipeline.IRTranslation.Invariant.CounterBound (counterInvariants) where

import Backend.Ast
import Backend.Utilities
import Data.Map qualified as M
import Pipeline.IRTranslation.Utilities

-- | Composes all counter invariants under conjunction
counterInvariants :: 𝛱 -> [Exp]
counterInvariants = map counterInvariant . M.keys

{- | Constrict the value of pc(π) over viable program points.
Depends on: π, 𝜙

Produces:

> 0 <= pc(π) ∧ pc(π) <= (max ∘ dom)(𝜙)
-}
counterInvariant :: P -> Exp
counterInvariant p =
  let pc = π p
      zero = (0 #)
      lower = zero :<= pc
      upper = pc :<= 𝜒 p
   in lower :&& upper