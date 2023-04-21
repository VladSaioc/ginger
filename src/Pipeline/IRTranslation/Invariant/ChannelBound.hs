module Pipeline.IRTranslation.Invariant.ChannelBound (channelBounds) where

import Backend.Ast
import Backend.Utilities
import Data.Map qualified as M
import Pipeline.IRTranslation.Utilities

{- Composes all channel bound invariants.
-}
channelBounds :: KEnv -> [Exp]
channelBounds = map channelBound . M.toList

{- Constructs a channel bound invariant.
Depends on: κ, c

Produces:
0 ≤ c ∧ c ≤ κ(c)
-}
channelBound :: (String, Exp) -> Exp
channelBound (c, e) =
  let lower = Leq (0 #) (c @)
      upper = Leq (c @) e
   in And lower upper