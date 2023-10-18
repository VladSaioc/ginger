module Pipeline.IRTranslation.Clauses.CommPrecondition (preconditions) where

import Backend.Ast
import Backend.Utilities
import Data.List qualified as L
import Data.Map qualified as M
import Data.Maybe qualified as Mb
import Debug.Trace
import IR.Utilities
import Pipeline.IRTranslation.Meta.Channel
import Pipeline.IRTranslation.Meta.Loop
import Pipeline.IRTranslation.Utilities
import Utilities.Collection

-- | Creates a call to the "iter" function on loop bounds.
--
-- Produces: 
--
-- > iter(lo,hi)
iterations :: Exp -> Exp -> Exp
iterations lo hi = Call "iter" [lo, hi]

-- | Create all channel preconditions.
preconditions :: 𝛹 -> 𝛫 -> P ↦ (𝐶 ↦ 𝒪s) -> [ℒ] -> [Exp]
preconditions 𝜓 𝜅 noloops loops =
  let -- Gather precondition contributions for every channel for
      -- operations in loop statements.
      lR = M.unionsWith (M.unionWith (:+)) (L.map (loopToPre 𝜓) loops)
      -- Gather precondition contributions for every channel for
      -- operations outside loops.
      nR = noloopOpToPre 𝜓 noloops
      -- Combine the sets of channel names.
      cs = L.union (M.keys lR) (M.keys nR)
      -- Construct a precondition for channel c.
      prc c =
        let -- Get channel capacity expression.
            k = Mb.fromJust (M.lookup c 𝜅)
            -- Get channel precondition sub-expressions from map.
            cR r = Mb.fromMaybe M.empty (M.lookup c r)
            -- Get precondition sub-expressions for channel operation direction.
            cdR d r = Mb.fromMaybe (0 #) (M.lookup d r)
            -- Get loop and non-loop precondition sub-expressions.
            (clR, cnR) = (cR lR, cR nR)

            -- Get send sub-expressions.
            sends = cdR S clR :+ cdR S cnR
            -- Get receive sub-expressions.
            rcvs = cdR R clR :+ cdR R cnR

            -- Construct receive unblock precondition.
            -- Receives unblock if there are more sends.
            rcvsUnblock = rcvs :<= sends
            -- Construct send unblock precondition.
            -- Sends unblock if there are more receive operations and
            -- capacity combined.
            sndsUnblock = sends :<= (rcvs :+ k)
         in -- The final precondition requires that both the receives and
            -- sends unblock. 
            rcvsUnblock :&& sndsUnblock
   in L.map prc cs

{- | Constructs the resource contribution resulting from
loop channel operations.
Depends on: ℓ = (x, e₁, e₂, O)

Reachability conditions for all processes:

>  𝜓 = [p ↦ [𝑛 ↦ e | 𝑛 ∈ dom(𝛯(p))] | p ∈ dom(𝛯)]

Produces:

> [ c ↦ [
>   ! ↦ |O! ⊆ O| * iterations(e₁, e₂)
>   ? ↦ |O? ⊆ O| * iterations(e₁, e₂)
> ] | c ∈ chans(O)]
-}
loopToPre :: 𝛹 -> ℒ -> 𝐶 ↦ (OpDir ↦ Exp)
loopToPre 𝜓 (ℒ {lP = p, l𝑛 = 𝑛, lower, upper, l𝒪s = os}) =
  let iter ops =
        let b = 𝜓 M.! p M.! 𝑛
            e = case length ops of
              0 -> (0 #)
              1 -> iterations lower upper
              n -> (n #) :* iterations lower upper
         in IfElse b e (0 #)
   in M.map (M.map iter) os

{- | Constructs the resource contribution resulting from
non-loop channel operations.
Depends on: O

Reachability conditions for all processes:

>  𝜓 = [p ↦ [𝑛 ↦ e | 𝑛 ∈ dom(𝛯(p))] | p ∈ dom(𝛯)]

Produces:

> [ c ↦ [
>   ! ↦ (𝛴 (p, 𝜙) ∈ dom(𝛯). ∀ 𝑛, (𝑛, c!) ∈ 𝜙. if 𝜓(p)(𝑛) then 1 else 0)
>   ? ↦ (𝛴 (p, 𝜙) ∈ dom(𝛯). ∀ 𝑛, (𝑛, c?) ∈ 𝜙. if 𝜓(p)(𝑛) then 1 else 0)
> ] | c ∈ chans(O)]
-}
noloopOpToPre :: 𝛹 -> P ↦ (𝐶 ↦ 𝒪s) -> 𝐶 ↦ (OpDir ↦ Exp)
noloopOpToPre 𝜓 pis =
  let chOp 𝒪 {oP = p, o𝑛 = 𝑛} =
        let b = 𝜓 M.! p M.! 𝑛
         in IfElse b (1 #) (0 #)
      pis' = (M.elems . M.map (M.map (M.map ((...+) . map chOp)))) pis
   in M.unionsWith (M.unionWith (:+)) pis'
