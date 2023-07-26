module Pipeline.IRTranslation.Invariant.Loop (loopMonitors) where

import Backend.Ast
import Backend.Utilities
import Pipeline.IRTranslation.Utilities

{- Get all loop monitor expressions.
-}
loopMonitors :: [Loop] -> [Exp]
loopMonitors = map loopMonitor

{- Constructs a loop monitor invariant.
Depends on: ℓ = (π, x, n, n', lo, hi, b)

Produces:
b => if lo ≤ hi then
      lo ≤ x ≤ hi
      pc(π) < n => x = lo ∧
      n < pc(π) < n' => x < hi ∧
      n' ≤ pc(π) => x = hi
    else x = e ∧ ¬(n < pc(π) < n')
-}
loopMonitor :: Loop -> Exp
loopMonitor (Loop {var, pid, guardP, exitP, lower, upper, pathexp}) =
  let lo = lower -- Short-hand for lower bound
      hi = upper -- Short-hand for upper bound
      -- Loop variable as a back-end variable
      x = (var @)
      -- Program counter as a back-end variable
      pc = π pid
      -- Loop guard point as a fixed program point
      guard = (guardP #)
      -- Loop exit point as a fixed program point
      exit = (exitP #)
      counterInLoop = And (Lt guard pc) (Lt pc exit)
      -- Initial guard checks whether the loop will be entered at all.
      -- If the lower bound is already strictly higher than the upper bound,
      -- then no iterations are performed.
      initGuard = Geq upper lower
      -- The clauses modeling loop behaviour when it has 0 or more
      -- iterations.
      hasIter =
        let -- Loop counter is bounded: lo ≤ x ≤ hi
            bounded = And (Leq lo x) (Leq x hi)
            -- Counter value before guard is reached: pc(π) < n => x = lo
            before = Implies (Lt pc guard) (Eq x lo)
            -- Counter value while loop is executing: n < pc(π) < n' => x < hi
            during = Implies counterInLoop (Lt x hi)
            -- Counter value after loop has executed: n' ≤ pc(π) => x = hi
            after = Implies (Leq exit pc) (Geq x hi)
         in Implies pathexp ([bounded, before, during, after] ...⋀)
      -- The clause modeling loop behaviour if the loop never enters.
      noIter =
        let -- The incrementing variable will remain frozen to the lower bound.
            bounded = Eq x lower
         in -- The loop counter will never reach loop body program points in these scenarios.
            And bounded (Not counterInLoop)
   in IfElse initGuard hasIter noIter