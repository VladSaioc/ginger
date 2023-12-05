module Pipeline.IRTranslation.Invariant.Return (returnMonitors) where

import Data.Map qualified as M

import Backend.Ast
import Backend.Utilities
import Pipeline.IRTranslation.Meta.Return
import Pipeline.IRTranslation.Utilities

{- Get all return monitor expressions.
-}
returnMonitors :: 𝛹 -> [ℛ] -> [Exp]
returnMonitors 𝜓 = map $ returnMonitor 𝜓

{- | Constructs a return monitor invariant.
Depends on: 𝜓, 𝑟 = (p, 𝑛)

Produces:

> 𝜓(p)(𝑛) ==> !(𝑛 < 𝜋(p) && 𝜋(p) < 𝜒(𝜋))
-}
returnMonitor :: 𝛹 -> ℛ -> Exp
returnMonitor 𝜓 (ℛ {rP = p, r𝑛 = 𝑛}) =
  let b = 𝜓 M.! p M.! 𝑛
   in b :==> Not (((𝑛 #) :< 𝜋 p) :&& (𝜋 p :< 𝜒 p))
