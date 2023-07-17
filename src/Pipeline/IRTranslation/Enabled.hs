module Pipeline.IRTranslation.Enabled (enabledExp) where

import Backend.Ast
import Backend.Utilities
import Data.List qualified as L
import Data.Map qualified as M
import Data.Maybe qualified as Mb
import IR.Utilities
import Pipeline.IRTranslation.ChInstructions (chanOps)
import Pipeline.IRTranslation.Utilities

{- Composes the enabled predicates for all processes
under disjunction.
Depends on: κ, Π

Produces:
⋁ (π, ϕ) ∈ Π. enabled(κ, π, ϕ)
-}
enabledExp :: KEnv -> Procs -> Exp
enabledExp kenv = (...⋁) . M.elems . M.mapWithKey (enabled kenv)

{- Computes an enabled predicate for a given process.
Depends on: κ, π, ϕ

Let E! = ⋀ (c, !, n) ∈ chanOps(ϕ). pc(π) = n => c < κ(c)
Let E? = ⋀ (c, ?, n) ∈ chanOps(ϕ). pc(π) = n => c > 0

Produces:
pc(π) < (max ∘ dom)(ϕ) ∧ E! ∧ E?
-}
enabled :: KEnv -> Pid -> ProgPoints -> Exp
enabled kenv pid pp =
  let pc = π pid
      chsops = chanOps pp
      notTerminated = Ne pc ((pp -|) #)
      subExp (cn, d, n) =
        let k = Mb.fromJust (M.lookup cn kenv)
            c = (cn @)
            executing = Implies . Eq pc . (#)
            async = Implies (Lt (0 #) k) . executing n
            sync = Implies (Eq (0 #) k)

            aEnabled = case d of
              S -> Lt c k
              R -> Gt c (0 #)

            sEnabled = case d of
              S ->
                let syncing = executing n $ Eq c (0 #)
                    rendezvous = executing (n + 1) $ Eq c ((-1) #)
                 in And syncing rendezvous
              R -> executing n $ Eq c (1 #)
         in And (async aEnabled) (sync sEnabled)
   in And notTerminated (L.map subExp chsops ...⋀)
