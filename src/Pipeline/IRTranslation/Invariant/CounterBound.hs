module Pipeline.IRTranslation.Invariant.CounterBound (counterInvariants) where

import Data.Map qualified as M

import Backend.Ast
import Backend.Utilities
import Pipeline.IRTranslation.Utilities

-- | Composes all counter invariants under conjunction
counterInvariants :: 𝛯 -> [Exp]
counterInvariants = map counterInvariant . M.keys

{- | Constrict the value of 𝜋(p) over viable program points.
Depends on: p, 𝜙

Produces:

> i <= 𝜋(p) ∧ 𝜋(p) <= 𝜒(p)
> where i = {
>   0, if p = 0,
>   i = -1, if p > 0
> }
-}
counterInvariant :: P -> Exp
counterInvariant p =
  let pc = 𝜋 p
      lower = ((if p == 0 then 0 else -1) #) :<= pc
      upper = pc :<= 𝜒 p
   in lower :&& upper
