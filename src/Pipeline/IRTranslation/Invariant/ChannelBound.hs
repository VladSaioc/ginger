module Pipeline.IRTranslation.Invariant.ChannelBound (channelBounds) where

import Backend.Ast
import Backend.Utilities
import Pipeline.IRTranslation.Summary.Chan

{- | Composes all channel bound invariants.
Depends on: 𝜅

Produces:

> ∀ c ∈ dom(𝜅). channelBound(c, 𝜅(c))
-}
channelBounds :: [𝒞] -> [Exp]
channelBounds = map channelBound

{- | Constructs a channel bound invariant.
Depends on: 𝜓, 𝒞 { 𝑛, p, c, e }

Produces:

> 𝜅(c) ≥ 0 ⟹ if 𝜅(c) > 0 then 0 ≤ c ∧ c ≤ 𝜅(c) else c in {1, 0, -1}
-}
channelBound :: 𝒞 -> Exp
channelBound 𝒞{c𝐶 = c, cCap = k} =
  let -- 0 ≤ c ∧ c ≤ 𝜅(c)
      asyncBound = (0 #) :<= (c @) :&& ((c @) :<= k)
      -- c ∈ {1, 0, -1}
      syncBound = In (c @) (ESet [(1 #), (0 #), ((-1) #)])
   in -- 𝜅(c) ≥ 0 ⟹ if 𝜅(c) > 0 then 0 ≤ c ∧ c ≤ 𝜅(c) else c in {1, 0, -1}
      (k :>= (0 #)) :==> IfElse (k :> (0 #)) asyncBound syncBound
