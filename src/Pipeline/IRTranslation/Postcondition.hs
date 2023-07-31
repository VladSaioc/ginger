module Pipeline.IRTranslation.Postcondition (postconditions) where

import Backend.Ast
import Data.Map qualified as M
import Pipeline.IRTranslation.Utilities

postconditions :: 𝛱 -> [Exp]
postconditions = map (uncurry postcondition) . M.toList

{- Constructs a postcondition for the given process.
Depends on: π, ϕ

Produces:
pc(π) = (max ∘ dom)(ϕ)
-}
postcondition :: P -> 𝛷 -> Exp
postcondition p 𝜙 = Eq (π p) (𝜙 -|)
