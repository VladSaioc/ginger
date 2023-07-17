module Pipeline.IRTranslation.Invariant.ChannelBound (channelBounds) where

import Backend.Ast
import Backend.Utilities
import Data.Map qualified as M
import Pipeline.IRTranslation.Utilities

{- Composes all channel bound invariants.
Depends on: κ

Produces:
∀ c ∈ dom(κ). channelBound(c, κ(c))
-}
channelBounds :: KEnv -> [Exp]
channelBounds = map (uncurry channelBound) . M.toList

{- Constructs a channel bound invariant.
Depends on: c, κ(c)

Produces:
(κ(c) > 0 ⇒ 0 ≤ c ∧ c ≤ κ(c)) ∧ (κ(c) = 0 ⇒ c ∈ {1, 0, -1})
-}
channelBound :: String -> Exp -> Exp
channelBound c k =
  let -- 0 ≤ c
      lower = Leq (0 #) (c @)
      -- c ≤ κ(c)
      upper = Leq (c @) k
      -- κ(c) = 0
      isSync = Eq k (0 #)
      -- κ(c) ≥ 0
      isAsync = Gt k (0 #)
      -- 0 ≤ c ∧ c ≤ κ(c)
      asyncBound = And lower upper
      -- c ∈ {1, 0, -1}
      syncBound = In (c @) (ESet [(1 #), (0 #), ((-1) #)])
   in And
        (Implies isSync syncBound)
        (Implies isAsync asyncBound)