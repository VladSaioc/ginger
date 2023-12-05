module Pipeline.IRTranslation.Clauses.Postcondition (postcondition) where

import Data.Map qualified as M

import Backend.Ast
import Backend.Utilities
import Pipeline.IRTranslation.Meta.Go
import Pipeline.IRTranslation.Utilities

{- | Constructs postconditions for all the processes.
Depends on: 𝜓, 𝛯, [g]

Produces:

> 𝜋(0) = (max ∘ dom)((𝛯)(0))
>  ∧ ⋀∀ (p, p', 𝑛) ∈ [g].
>     𝜋(p') = (if 𝜓(p)(𝑛) then (max ∘ dom)((𝛯)(p')) else -1)
-}
postcondition :: 𝛹 -> 𝛯 -> [𝒢] -> Exp
postcondition 𝜓 𝜉 gs =
  (((𝜋 0 :== ((𝜉 M.! 0) -|)) : map (goPostcondition 𝜓 𝜉) gs)  ...⋀)

{- | Constructs a postcondition from a given go instruction.
Depends on: 𝜓, 𝛯, g = (p, p', 𝑛).

Produces:

> 𝜋(p') = (if 𝜓(p)(𝑛) then (max ∘ dom)((𝛯)(p')) else -1)
-}
goPostcondition :: 𝛹 -> 𝛯 -> 𝒢 -> Exp
goPostcondition 𝜓 𝜉 𝒢 { gP = p, gP' = p', g𝑛 = 𝑛 } =
  let 𝜙 = 𝜉 M.! p'
   in 𝜋 p' :== IfElse (𝜓 M.! p M.! 𝑛) (𝜙 -|) ((-1) #)
