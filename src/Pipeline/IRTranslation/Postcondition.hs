module Pipeline.IRTranslation.Postcondition (postconditions) where

import Backend.Ast
import Backend.Utilities
import Data.Map qualified as M
import Pipeline.IRTranslation.Utilities

postconditions :: Procs -> [Exp]
postconditions = map (uncurry postcondition) . M.toList

{- Constructs a postcondition for the given process.
Depends on: π, ϕ

Produces:
pc(π) = (max ∘ dom)(ϕ)
-}
postcondition :: Pid -> ProgPoints -> Exp
postcondition pid pp = Eq (π pid) ((pp -|) #)
