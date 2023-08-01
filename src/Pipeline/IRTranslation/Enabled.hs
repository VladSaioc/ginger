module Pipeline.IRTranslation.Enabled (enabledExp) where

import Backend.Ast
import Backend.Utilities
import Data.List qualified as L
import Data.Map qualified as M
import Data.Maybe qualified as Mb
import IR.Utilities
import Pipeline.IRTranslation.Meta.Channel
import Pipeline.IRTranslation.Utilities

{- Aggregate all channel operation points from a given map of program points.
Produces a list of channel operation metadata, including the channel name,
process id, operation direction, program point.
Depends on: ϕ

Produces:
{ (c, d, 𝑛) | (𝑛, cd) ∈ ϕ \ loop(ϕ). d ∈ {!, ?} }
-}
chanOps :: P -> 𝛷 -> [𝒪]
chanOps p =
  let insn 𝑛 s = do
        op <- backendChannelOp s
        let (c, d) = either (,S) (,R) op
        return
          𝒪
            { oP = p,
              o𝐶 = c,
              oDir = d,
              o𝑛 = 𝑛,
              oPathexp = (True ?)
            }
   in Mb.catMaybes . M.elems . M.mapWithKey insn

{- Composes the enabled predicates for all processes
under disjunction.
Depends on: κ, Π

Produces:
⋁ (π, ϕ) ∈ Π. enabled(κ, π, ϕ)
-}
enabledExp :: K -> 𝛱 -> Exp
enabledExp κ = (...⋁) . M.elems . M.mapWithKey (enabled κ)

{- Computes an enabled predicate for a given process.
Depends on: κ, π, ϕ

Let E! = ⋀ (c, !, 𝑛) ∈ chanOps(ϕ). pc(π) = 𝑛 => c < κ(c)
Let E? = ⋀ (c, ?, 𝑛) ∈ chanOps(ϕ). pc(π) = 𝑛 => c > 0

Produces:
pc(π) < (max ∘ dom)(ϕ) ∧ E! ∧ E?
-}
enabled :: K -> P -> 𝛷 -> Exp
enabled κ p 𝜙 =
  let -- Process id variable
      pc = π p
      -- Construct match over process id
      match cs = Match pc (cs ++ [(Wildcard, pc :< (𝜙 -|))])
      chsops = chanOps p 𝜙
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
