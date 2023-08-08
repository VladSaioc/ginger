module Pipeline.IRTranslation.Invariant.Return (returnMonitors) where

import Backend.Ast
import Backend.Utilities
import Data.Map qualified as M
import Pipeline.IRTranslation.Meta.Return
import Pipeline.IRTranslation.Utilities

{- Get all if monitor expressions.
-}
returnMonitors :: 𝛹 -> [ℛ] -> [Exp]
returnMonitors 𝜓 = map $ returnMonitor 𝜓

{- Constructs a return monitor invariant.
Depends on: 𝑟 = (π, n, e)

Produces:
e ==> !(n < pc(π) && pc(π) < exit(π))
-}
returnMonitor :: 𝛹 -> ℛ -> Exp
returnMonitor 𝜓 (ℛ {rP = p, r𝑛 = 𝑛}) =
  let b = 𝜓 M.! p M.! 𝑛
   in b :==> Not (((𝑛 #) :< π p) :&& (π p :< 𝜒 p))