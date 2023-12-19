module Pipeline.IRTranslation.Invariant.ChannelDef (channelDefs) where

import Data.Map qualified as M

import Backend.Ast
import Backend.Utilities
import Pipeline.IRTranslation.Clauses.Utilities
import Pipeline.IRTranslation.Summary.Chan
import Pipeline.IRTranslation.Utilities

{- | Composes all channel definition invariants.
It monitors whether an error has been triggered by a reachable
channel definition with a negative capacity.

Produces:

> ERR <==> ⋁∀ 𝒞 { 𝑛, p, c, 𝜅(c) } ∈ [𝒞]. 𝜓(p, 𝑛) ∧ 𝜋(p) > 𝑛 ∧ 𝜅(c) < 0
-}
channelDefs :: 𝛹 -> [𝒞] -> Exp
channelDefs 𝜓 cs = (𝑥ERR @) :<==> (map (channelDef 𝜓) cs ...⋁)

{- | Constructs a channel definition invariant.
Depends on: 𝜓, 𝒞 { 𝑛, p, c, 𝜅(c) }

Produces:

> 𝜓(p, 𝑛) ∧ 𝜋(p) > 𝑛 ∧ 𝜅(c) < 0
-}
channelDef :: 𝛹 -> 𝒞 -> Exp
channelDef 𝜓 𝒞{cP = p, cCap = k, c𝑛 = 𝑛} =
  (𝜓 M.! p M.! 𝑛) :&& (𝜋 p :> (𝑛 #)) :&& (k :< (0 #))
