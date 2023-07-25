module Pipeline.IRTranslation.Invariant.CounterBound (counterInvariants) where

import Backend.Ast
import Data.Map qualified as M
import Pipeline.IRTranslation.Utilities

-- Composes all counter invariants under conjunction
counterInvariants :: Procs -> [Exp]
counterInvariants = map counterInvariant . M.toList

{- Constrict the value of pc(π) over viable program points.
Depends on: π, ϕ

Produces:
0 <= pc(π) ∧ pc(π) <= (max ∘ dom)(ϕ)
-}
counterInvariant :: (Pid, ProgPoints) -> Exp
counterInvariant (pid, pp) =
  let terminated = (pp -|)
      pc = EVar (pid <|)
      zero = ECon (CNum 0)
      lower = Leq zero pc
      upper = Leq pc terminated
   in And lower upper