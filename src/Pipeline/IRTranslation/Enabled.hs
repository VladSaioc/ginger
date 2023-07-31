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
{ (c, d, n) | (n, cd) ∈ ϕ \ loop(ϕ). d ∈ {!, ?} }
-}
chanOps :: P -> 𝛷 -> [𝒪]
chanOps pid =
  let insn 𝑛 s = do
        op <- backendChannelOp s
        let (c, d) = either (,S) (,R) op
        return
          𝒪
            { oP = pid,
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

Let E! = ⋀ (c, !, n) ∈ chanOps(ϕ). pc(π) = n => c < κ(c)
Let E? = ⋀ (c, ?, n) ∈ chanOps(ϕ). pc(π) = n => c > 0

Produces:
pc(π) < (max ∘ dom)(ϕ) ∧ E! ∧ E?
-}
enabled :: K -> P -> 𝛷 -> Exp
enabled κ p 𝜙 =
  let pc = π p
      chsops = chanOps p 𝜙
      notTerminated = pc :!= (𝜙 -|)
      subExp 𝒪 {o𝐶 = cn, o𝑛 = 𝑛, oDir = d} =
        let k = Mb.fromJust (M.lookup cn κ)
            c = (cn @)
            executing = (:==>) . (:==) pc . (#)

            aEnabled = executing 𝑛 $ case d of
              S -> c :< k
              R -> c :> (0 #)

            sEnabled = case d of
              S ->
                let syncing = executing 𝑛 $ c :== (0 #)
                    rendezvous = executing (𝑛 + 1) $ c :== ((-1) #)
                 in syncing :&& rendezvous
              R -> executing 𝑛 $ c :== (1 #)
         in IfElse ((0 #) :< k) aEnabled sEnabled
   in notTerminated :&& (L.map subExp chsops ...⋀)
