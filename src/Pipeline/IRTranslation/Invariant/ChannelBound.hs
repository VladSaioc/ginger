module Pipeline.IRTranslation.Invariant.ChannelBound (buildChBoundInvs) where

import Backend.Ast
import Data.Map qualified as M
import Pipeline.IRTranslation.Utilities

buildChBoundInvs :: KEnv -> [Exp]
buildChBoundInvs = map buildChBoundInv . M.toList

buildChBoundInv :: (String, Exp) -> Exp
buildChBoundInv (c, e) =
  let c' = EVar c
      zero = ECon (CNum 0)
      lower = Leq zero c'
      upper = Leq c' e
   in And lower upper