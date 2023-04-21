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
enabledExp kenv = (.\/.) . M.elems . M.mapWithKey (enabled kenv)

{- Computes an enabled predicate for a given process.
Depends on: κ, π, ϕ

Let E! = ⋀ (c, !, n) ∈ chanOps(ϕ). pc(π) = n => c < κ(c)
Let E? = ⋀ (c, ?, n) ∈ chanOps(ϕ). pc(π) = n => c > 0

Produces:
pc(π) < (max ∘ dom)(ϕ) ∧ E! ∧ E?
-}
enabled :: KEnv -> Pid -> ProgPoints -> Exp
enabled kenv pid pp =
  let pc = ((pid <|) @)
      chsops = chanOps pp
      notTerminated = Ne pc ((pp -|) #)
      subExp (c, d, opc) =
        let executing = Eq pc (opc #)
            unblocked = case d of
              S -> Lt (c @) (Mb.fromJust (M.lookup c kenv))
              R -> Gt (c @) (0 #)
         in Implies executing unblocked
   in And notTerminated (L.map subExp chsops ./\.)
