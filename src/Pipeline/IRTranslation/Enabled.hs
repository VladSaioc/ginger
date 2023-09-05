module Pipeline.IRTranslation.Enabled (enabledExp) where

import Backend.Ast
import Backend.Utilities
import Data.Map qualified as M
import Data.Maybe qualified as Mb
import IR.Utilities
import Pipeline.IRTranslation.Meta.Channel
import Pipeline.IRTranslation.Utilities

{- | Composes the enabled predicates for all processes
under disjunction.
Depends on: κ, 𝛯

Produces:

> ⋁ (p, 𝜙) ∈ 𝛯. enabled(κ, p, 𝜙)
-}
enabledExp :: K -> 𝛯 -> Exp
enabledExp κ = (...⋁) . M.elems . M.mapWithKey (enabled κ)

{- | Computes an enabled predicate for a given process.
Depends on: κ, p, 𝜙

Let the following:

> C! = ⋃ ∀ (c, !, 𝑛) ∈ chanOps(𝜙). [
>    𝑛 ↦ if 0 < κ(c) then c < κ(c) else c == 0,
>    (𝑛 + 1) ↦ c == -1
>  ]
> C? = ⋃ ∀ (c, !, 𝑛) ∈ chanOps(𝜙). [
>    𝑛 ↦ if 0 < κ(c) then c > 0 else c == 1
>  ]

Produces:

> match 𝜋(p) {
> ∀ (𝑛, e) ∈ C! ∪ C?. case 𝑛 => e
> case _ => -1 < 𝜋(p) < (max ∘ dom)(𝜙)
> }
-}
enabled :: K -> P -> 𝛷 -> Exp
enabled κ p 𝜙 =
  let -- Process id variable
      pc = 𝜋 p
      -- Construct match over process id
      match cs = Match pc (cs ++ [(Wildcard, ((-1) #) :< pc :< 𝜒 p)])
      chsops = processChanOps p 𝜙
      -- Process has not reached termination point
      subExp 𝒪 {o𝐶 = cn, o𝑛 = 𝑛, oDir = d} =
        let k = Mb.fromJust (M.lookup cn κ)
            c = (cn @)

            -- If the process is at instruction 𝑛', check e
            -- case 𝑛' => e
            executing 𝑛' e = (PCon (CNum 𝑛'), e)
            -- Check for the buffered case for capacity k:
            -- if 0 < k then e1 else e2
            bufCase = IfElse ((0 #) :< k)

            opEnabled = case d of
              -- Send operations are enabled if:
              -- 1. Buffered case: the channel is not full
              -- 2. Unbuffered case:
              -- -- The channel is available to write to.
              -- -- Rendezvous has been established
              S ->
                [ executing 𝑛 $ bufCase (c :< k) (c :== (0 #)),
                  executing (𝑛 + 1) (c :== ((-1) #))
                ]
              -- Receive operations are enabled if:
              -- 1. Buffered case: the channel is not empty
              -- 2. Unbuffered case: the channel is available to read from.
              R ->
                [ executing 𝑛 $ bufCase (c :> (0 #)) (c :== (1 #))
                ]
         in opEnabled
   in match (concatMap subExp chsops)
