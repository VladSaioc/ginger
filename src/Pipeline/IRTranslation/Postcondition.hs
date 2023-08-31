module Pipeline.IRTranslation.Postcondition (postconditions) where

import Backend.Ast
import Data.Map qualified as M
import Pipeline.IRTranslation.Utilities

postconditions :: 𝛱 -> [Exp]
postconditions = map (uncurry postcondition) . M.toList

{- | Constructs a postcondition for the given process.
Depends on: π, 𝜙

Produces:

> pc(π) = (max ∘ dom)(𝜙)
-}
postcondition :: P -> 𝛷 -> Exp
postcondition p 𝜙 = π p :== (𝜙 -|)
