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
Depends on: 𝒾 = (π, e, n, n', n'')

Produces:
if e then !(n' < pc(π) < n'') else !(n < pc(π) < n')
-}
ifMonitor :: ℐ -> Exp
ifMonitor (ℐ {iP = pid, iGuardExp = b, iGuard = 𝑛, iElse = 𝑛', iExit = 𝑛''}) =
  let -- Program counter as a back-end variable
      pc = π pid
      -- If guard point as a fixed program point
      guard = (𝑛 #)
      -- If else point as a fixed program point
      els = (𝑛' #)
      -- If exit point as a fixed program point
      exit = (𝑛'' #)
      counterInThen = (guard :< pc) :&& (pc :< els)
      counterInElse = (els :<= pc) :&& (pc :< exit)
   in IfElse b (Not counterInElse) (Not counterInThen)