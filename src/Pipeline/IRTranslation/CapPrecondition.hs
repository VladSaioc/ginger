module Pipeline.IRTranslation.CapPrecondition (capPreconditions) where

import Backend.Ast
import Backend.Utilities
import Data.List qualified as L
import Data.Map qualified as M
import Pipeline.IRTranslation.Utilities

-- Aggregate all asynchrony preconditions.
capPreconditions :: KEnv -> [Exp]
capPreconditions = L.map capPrecondition . M.elems

{- Constructs a precondition certifying that the channel capacity
is a valid expression i.e., a positive integer.
Depends on: κ, c

Produces:
κ(c) ≥ 0
-}
capPrecondition :: Exp -> Exp
capPrecondition = Leq (0 #)