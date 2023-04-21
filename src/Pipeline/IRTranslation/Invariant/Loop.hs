module Pipeline.IRTranslation.Invariant.Loop (loopMonitors) where

import Backend.Ast
import Backend.Utilities
import Pipeline.IRTranslation.Utilities

loopMonitors :: [Loop] -> [Exp]
loopMonitors = map loopMonitor

{- Constructs a loop monitor invariant.
Depends on: ℓ = (π, x, n, n', e, e')

Produces:
if lo ≤ hi then
  pc(π) < n => x = e ∧
  n < pc(π) < n' => x < e ∧
  n' ≤ pc(π) => x = e'
else x = e ∧ ¬(n < pc(π) < n')
-}
loopMonitor :: Loop -> Exp
loopMonitor (Loop {var, pid, guardP, exitP, lower, upper}) =
  let x = EVar var
      pc = EVar (pid <|)
      guard = (guardP #)
      exit = (exitP #)
      initGuard = Geq upper lower
      hasIter =
        let bounded = And (Leq lower x) (Leq x upper)
            before = Implies (Lt pc exit) (Eq x lower)
            during = Implies (And (Lt guard pc) (Lt pc exit)) (Lt x upper)
            after = Implies (Leq exit pc) (Geq x upper)
         in arithmCompose And [bounded, before, during, after]
      noIter =
        let bounded = Eq x lower
            body = And (Lt guard pc) (Lt pc exit)
         in And bounded (Not body)
   in IfElse initGuard hasIter noIter