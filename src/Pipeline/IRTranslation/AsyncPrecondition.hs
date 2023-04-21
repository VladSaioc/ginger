module Pipeline.IRTranslation.AsyncPrecondition (asyncPreconditions) where

import Backend.Ast
import Backend.Utilities
import Data.List qualified as L
import Data.Map qualified as M
import Pipeline.IRTranslation.Utilities

-- loopPreconditions :: Procs -> [Exp]

-- loopPreconditions pp

-- Aggregate all asynchrony preconditions.
asyncPreconditions :: KEnv -> [Exp]
asyncPreconditions = L.map asyncPrecondition . M.elems

{- Constructs a precondition certifying channel asynchrony.
Depends on: κ, c

Produces:
κ(c) > 0
-}
asyncPrecondition :: Exp -> Exp
asyncPrecondition = Lt (0 #)