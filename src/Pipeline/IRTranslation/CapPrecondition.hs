module Pipeline.IRTranslation.CapPrecondition (capPreconditions) where

import Backend.Ast
import Backend.Utilities
import Data.List qualified as L
import Data.Map qualified as M
import Pipeline.IRTranslation.Meta.Channel

-- Aggregate all asynchrony preconditions.
capPreconditions :: K -> [Exp]
capPreconditions = L.map capPrecondition . M.elems

{- Constructs a precondition guaranteeing that the channel capacity
is valid i.e., its expression evaluates to a positive integer.
Depends on: κ, c

Produces:
κ(c) ≥ 0
-}
capPrecondition :: Exp -> Exp
capPrecondition = (:<=) (0 #)