module Pipeline.IRTranslation.CapPrecondition (capPreconditions) where

import Backend.Ast
import Backend.Utilities
import Data.List qualified as L
import Data.Map qualified as M
import Pipeline.IRTranslation.Utilities

-- loopPreconditions :: Procs -> [Exp]

-- loopPreconditions pp

-- Aggregate all asynchrony preconditions.
capPreconditions :: KEnv -> [Exp]
capPreconditions = L.map asyncPrecondition . M.elems

{- Constructs a precondition certifying that the channel capacity
is a valid expression i.e., a positive integer.
Depends on: κ, c

Produces:
κ(c) ≥ 0
-}
asyncPrecondition :: Exp -> Exp
asyncPrecondition = Leq (0 #)