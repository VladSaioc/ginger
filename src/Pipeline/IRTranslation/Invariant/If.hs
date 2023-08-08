module Pipeline.IRTranslation.Invariant.If (ifMonitors) where

import Backend.Ast
import Backend.Utilities
import Pipeline.IRTranslation.Meta.If
import Pipeline.IRTranslation.Utilities

{- Get all if monitor expressions.
-}
ifMonitors :: [ℐ] -> [Exp]
ifMonitors = map ifMonitor

{- Constructs an if monitor invariant.
Depends on: 𝒾 = (π, e, 𝑛₁, 𝑛₂, 𝑛₃)

Produces:
if e then ¬(𝑛₂ ≤ pc(π) < 𝑛₃) else ¬(𝑛₁ < pc(π) < 𝑛₂)
-}
ifMonitor :: ℐ -> Exp
ifMonitor (ℐ {iP = pid, iGuard = b, i𝑛 = 𝑛₁, iElse = 𝑛₂, iExit = 𝑛₃}) =
  let -- Program counter as a back-end variable
      pc = π pid
      -- If guard point as a fixed program point
      guard = (𝑛₁ #)
      -- If else point as a fixed program point
      els = (𝑛₂ #)
      -- If exit point as a fixed program point
      exit = (𝑛₃ #)
      counterInThen = (guard :< pc) :&& (pc :< els)
      counterInElse = (els :<= pc) :&& (pc :< exit)
   in IfElse b (Not counterInElse) (Not counterInThen)