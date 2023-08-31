module Pipeline.IRTranslation.Invariant.ChannelBound (channelBounds) where

import Backend.Ast
import Backend.Utilities
import Data.Map qualified as M
import Pipeline.IRTranslation.Meta.Channel

{- | Composes all channel bound invariants.
Depends on: κ

Produces:

> ∀ c ∈ dom(κ). channelBound(c, κ(c))
-}
channelBounds :: K -> [Exp]
channelBounds = map (uncurry channelBound) . M.toList

{- | Constructs a channel bound invariant.
Depends on: c, κ(c)

Produces:

> (κ(c) > 0 ⇒ 0 ≤ c ∧ c ≤ κ(c)) ∧ (κ(c) = 0 ⇒ c ∈ {1, 0, -1})
-}
channelBound :: String -> Exp -> Exp
channelBound c k =
  let -- 0 ≤ c
      lower = (0 #) :<= (c @)
      -- c ≤ κ(c)
      upper = (c @) :<= k
      -- κ(c) = 0
      isSync = k :== (0 #)
      -- 0 ≤ c ∧ c ≤ κ(c)
      asyncBound = lower :&& upper
      -- c ∈ {1, 0, -1}
      syncBound = In (c @) (ESet [(1 #), (0 #), ((-1) #)])
   in IfElse isSync syncBound asyncBound
