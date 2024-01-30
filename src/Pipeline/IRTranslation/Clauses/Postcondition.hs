module Pipeline.IRTranslation.Clauses.Postcondition (postcondition) where

import Backend.Ast
import Backend.Utilities
import Pipeline.IRTranslation.Summary.Go
import Pipeline.IRTranslation.Utilities

{- | Constructs postconditions for all the processes.
Depends on: 𝜓, 𝛯, [g]

Produces:

> 𝜋(0) = 𝜏(0) ∧ ⋀ ∀ (p', p, 𝑛) ∈ [g]. 𝜋(p) ≠ -1 ⟹ 𝜋(p) = 𝜏(p)
-}
postcondition :: [𝒢] -> Exp
postcondition gs = (((𝜋 0 :== 𝜏 0) : map goPostcondition gs)  ...⋀)

{- | Constructs a postcondition from a given go instruction.
Depends on: 𝜓, 𝛯, g = (p₀, p, 𝑛).

Produces:

> 𝜋(p) ≠ -1 ⟹ 𝜋(p) == 𝜏(p)
-}
goPostcondition :: 𝒢 -> Exp
goPostcondition 𝒢 { gP' = p' } = (𝜋 p' :!= ((-1) #)) :==> (𝜋 p' :== 𝜏 p')
