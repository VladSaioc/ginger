module Pipeline.IRTranslation.Invariant.Loop (loopMonitors) where

import Backend.Ast
import Backend.Utilities
import Pipeline.IRTranslation.Meta.Loop
import Pipeline.IRTranslation.Utilities

{- Get all loop monitor expressions.
-}
loopMonitors :: [ℒ] -> [Exp]
loopMonitors = map loopMonitor

{- Constructs a loop monitor invariant.
Depends on: ℓ = (π, x, n, n', lo, hi, b)

Produces:
if b && lo ≤ hi then
  lo ≤ x ≤ hi
  pc(π) < n => x = lo ∧
  n < pc(π) < n' => x < hi ∧
  n' ≤ pc(π) => x = hi
else x = e ∧ ¬(n < pc(π) < n')
-}
loopMonitor :: ℒ -> Exp
loopMonitor (ℒ {l𝑋 = var, lP = p, lGuard = g, lExit = ex, lower, upper, lPathexp = b}) =
  let lo = lower -- Short-hand for lower bound
      hi = upper -- Short-hand for upper bound
      -- Loop variable as a back-end variable
      x = (var @)
      -- Program counter as a back-end variable
      pc = π p
      -- Loop guard point as a fixed program point
      guard = (g #)
      -- Loop exit point as a fixed program point
      exit = (ex #)
      counterInLoop = (guard :< pc) :&& (pc :< exit)
      -- Initial guard checks whether the loop will be entered at all.
      -- If the lower bound is already strictly higher than the upper bound,
      -- or the loop is unreachable due to path conditions, then no iterations
      -- are performed.
      initGuard = b :&& (upper :>= lower)
      -- The clauses modeling loop behaviour when it has 0 or more
      -- iterations.
      hasIter =
        let -- Loop counter is bounded: lo ≤ x ≤ hi
            bounded = (lo :<= x) :&& (x :<= hi)
            -- Counter value before guard is reached: pc(π) < n => x = lo
            before = (pc :< guard) :==> (x :== lo)
            -- Counter value while loop is executing: n < pc(π) < n' => x < hi
            during = counterInLoop :==> (x :< hi)
            -- Counter value after loop has executed: n' ≤ pc(π) => x = hi
            after = (exit :<= pc) :==> (x :>= hi)
         in ([bounded, before, during, after] ...⋀)
      -- The clause modeling loop behaviour if the loop never enters.
      noIter =
        let -- The incrementing variable will remain frozen to the lower bound.
            bounded = x :== lower
         in -- The loop counter will never reach loop body program points in these scenarios.
            bounded :&& Not counterInLoop
   in IfElse initGuard hasIter noIter