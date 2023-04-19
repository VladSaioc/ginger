module Pipeline.IRTranslation.Invariant.CounterBound (buildPCBoundInvs) where

import Backend.Ast
import Data.Map qualified as M
import Pipeline.IRTranslation.Utilities

buildPCBoundInvs :: Procs -> [Exp]
buildPCBoundInvs = map buildPCBoundInv . M.toList

buildPCBoundInv :: (Integer, ProgPoints) -> Exp
buildPCBoundInv (pid, pp) =
  let terminated = ECon (CNum (end pp))
      pc = EVar (pid <|)
      zero = ECon (CNum 0)
      lower = Leq zero pc
      upper = Leq pc terminated
   in And lower upper