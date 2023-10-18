module Pipeline.IRTranslation.Invariant.ChannelBound (channelBounds) where

import Backend.Ast
import Backend.Utilities
import Data.Map qualified as M
import Pipeline.IRTranslation.Meta.Channel

{- | Composes all channel bound invariants.
Depends on: 𝜅

Produces:

> ∀ c ∈ dom(𝜅). channelBound(c, 𝜅(c))
-}
channelBounds :: 𝛫 -> [Exp]
channelBounds = map (uncurry channelBound) . M.toList

{- | Constructs a channel bound invariant.
Depends on: c, 𝜅(c)

Produces:

> (𝜅(c) > 0 ⇒ 0 ≤ c ∧ c ≤ 𝜅(c)) ∧ (𝜅(c) = 0 ⇒ c ∈ {1, 0, -1})
-}
channelBound :: String -> Exp -> Exp
channelBound c k =
  let -- 0 ≤ c
      lower = (0 #) :<= (c @)
      -- c ≤ 𝜅(c)
      upper = (c @) :<= k
      -- 𝜅(c) = 0
      isSync = k :== (0 #)
      -- 0 ≤ c ∧ c ≤ 𝜅(c)
      asyncBound = lower :&& upper
      -- c ∈ {1, 0, -1}
      syncBound = In (c @) (ESet [(1 #), (0 #), ((-1) #)])
   in IfElse isSync syncBound asyncBound
