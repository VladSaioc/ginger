module Pipeline.IRTranslation.Invariant.Loop (loopMonitors) where

import Backend.Ast
import Backend.Utilities
import Data.Map qualified as M
import Pipeline.IRTranslation.Meta.Loop
import Pipeline.IRTranslation.Utilities
import Utilities.Collection

{- Get all loop monitors for every loop.
-}
loopMonitors :: P ↦ (𝑁 ↦ Exp) -> [ℒ] -> [Exp]
loopMonitors 𝜓 = map (loopMonitor 𝜓)

{- Constructs a loop monitor invariant.
Depends on:
I. Reachability conditions for all processes:
    𝜓 = [π ↦ [𝑛 ↦ e | 𝑛 ∈ dom(𝛱(π))] | π ∈ dom(𝛱)]

II. ℓ = (π, x, 𝑛, 𝑛', e₁, e₂) with the following properties:

1. π is the process id of the loop
2. x is the loop index variable
3. e₁ is the lower bound expression
4. e₂ is the upper bound expression
5. 𝑛 is the guard point
6. 𝑛' is the exit point

Produces:
if 𝜓(π)(𝑛) && e₁ ≤ e₂ then
  e₁ ≤ x ≤ e₂
  pc(π) < 𝑛 => x = e₁ ∧
  𝑛 < pc(π) < 𝑛' => x < e₂ ∧
  𝑛' ≤ pc(π) => x = e₂
else x = e₁ ∧ ¬(𝑛 < pc(π) < 𝑛')
-}
loopMonitor :: P ↦ (𝑁 ↦ Exp) -> ℒ -> Exp
loopMonitor 𝜓 (ℒ {l𝑋 = var, lP = p, l𝑛 = 𝑛, lExit = 𝑛', lower, upper}) =
  let b = 𝜓 M.! p M.! 𝑛
      lo = lower -- Short-hand for lower bound
      hi = upper -- Short-hand for upper bound
      -- Loop variable as a back-end variable
      x = (var @)
      -- Program counter as a back-end variable
      pc = π p
      -- Loop guard point as a fixed program point
      guard = (𝑛 #)
      -- Loop exit point as a fixed program point
      exit = (𝑛' #)
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