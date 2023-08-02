module Pipeline.IRTranslation.Enabled (enabledExp) where

import Backend.Ast
import Backend.Utilities
import Data.Map qualified as M
import Data.Maybe qualified as Mb
import IR.Utilities
import Pipeline.IRTranslation.Meta.Channel
import Pipeline.IRTranslation.Utilities

{- Composes the enabled predicates for all processes
under disjunction.
Depends on: κ, Π

Produces:
⋁ (π, 𝜙) ∈ Π. enabled(κ, π, 𝜙)
-}
enabledExp :: K -> 𝛱 -> Exp
enabledExp κ = (...⋁) . M.elems . M.mapWithKey (enabled κ)

{- Computes an enabled predicate for a given process.
Depends on: κ, π, 𝜙

Let C! = ⋃ ∀ (c, !, 𝑛) ∈ chanOps(𝜙). {
    case 𝑛 => if 0 < κ(c) then c < κ(c) else c == 0,
    case (𝑛 + 1) => c == -1
  }
Let C? = ⋃ ∀ (c, !, 𝑛) ∈ chanOps(𝜙). {
    case 𝑛 => if 0 < κ(c) then c > 0 else c == 1
  }

Produces:
match pc(π) {
∀ c ∈ C!. c
∀ c ∈ C?. c
case _ => pc(π) < (max ∘ dom)(𝜙)
}
-}
enabled :: K -> P -> 𝛷 -> Exp
enabled κ p 𝜙 =
  let -- Process id variable
      pc = π p
      -- Construct match over process id
      match cs = Match pc (cs ++ [(Wildcard, pc :< (𝜙 -|))])
      chsops = processChanOps p 𝜙
      -- Process has not reached termination point
      subExp 𝒪 {o𝐶 = cn, o𝑛 = 𝑛, oDir = d} =
        let k = Mb.fromJust (M.lookup cn κ)
            c = (cn @)

            executing 𝑛' e = (PCon (CNum 𝑛'), e)
            bufCase = IfElse ((0 #) :< k)

            opEnabled = case d of
              S ->
                [ executing 𝑛 $ bufCase (c :< k) (c :== (0 #)),
                  executing (𝑛 + 1) (c :== ((-1) #))
                ]
              R ->
                [ executing 𝑛 $ bufCase (c :> (0 #)) (c :== (1 #))
                ]
         in opEnabled
   in match (concatMap subExp chsops)
