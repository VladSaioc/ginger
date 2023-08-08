module Pipeline.IRTranslation.CommPrecondition (preconditions) where

import Backend.Ast
import Backend.Utilities
import Data.List qualified as L
import Data.Map qualified as M
import Data.Maybe qualified as Mb
import IR.Utilities
import Pipeline.IRTranslation.Meta.Channel
import Pipeline.IRTranslation.Meta.Loop
import Pipeline.IRTranslation.Utilities
import Utilities.Collection

iterations :: Exp -> Exp -> Exp
iterations lo hi = Call "iter" [lo, hi]

preconditions :: 𝛹 -> K -> P ↦ (𝐶 ↦ 𝒪s) -> [ℒ] -> [Exp]
preconditions 𝜓 κ noloops loops =
  let lR = M.unionsWith (M.unionWith (:+)) (L.map (loopToPre 𝜓) loops)
      nR = noloopOpToPre 𝜓 noloops
      cs = L.union (M.keys lR) (M.keys nR)
      prc c =
        let k = Mb.fromJust (M.lookup c κ)
            cR r = Mb.fromMaybe M.empty (M.lookup c r)
            cdR d r = Mb.fromMaybe (0 #) (M.lookup d r)
            (clR, cnR) = (cR lR, cR nR)

            sends = cdR S clR :+ cdR S cnR
            rcvs = cdR R clR :+ cdR R cnR

            rcvsUnblock = rcvs :<= sends
            sndsUnblock = sends :<= (rcvs :+ k)
         in rcvsUnblock :&& sndsUnblock
   in L.map prc cs

{- Constructs the resource contribution resulting from
loop channel operations.
Depends on: ℓ = (x, e₁, e₂, O)
Reachability conditions for all processes:
  𝜓 = [π ↦ [𝑛 ↦ e | 𝑛 ∈ dom(𝛱(π))] | π ∈ dom(𝛱)]

Produces:
[ c ↦ [
  ! ↦ |O! ⊆ O| * iterations(e₁, e₂)
  ? ↦ |O? ⊆ O| * iterations(e₁, e₂)
] | c ∈ chans(O)]
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

{- Constructs the resource contribution resulting from
non-loop channel operations.
Depends on: O
Reachability conditions for all processes:
  𝜓 = [π ↦ [𝑛 ↦ e | 𝑛 ∈ dom(𝛱(π))] | π ∈ dom(𝛱)]

Produces:
[ c ↦ [
  ! ↦ (𝛴 (π, 𝜙) ∈ dom(𝛱). ∀ 𝑛, (𝑛, c!) ∈ 𝜙. if 𝜓(π)(𝑛) then 1 else 0)
  ? ↦ (𝛴 (π, 𝜙) ∈ dom(𝛱). ∀ 𝑛, (𝑛, c?) ∈ 𝜙. if 𝜓(π)(𝑛) then 1 else 0)
] | c ∈ chans(O)]
-}
noloopOpToPre :: 𝛹 -> P ↦ (𝐶 ↦ 𝒪s) -> 𝐶 ↦ (OpDir ↦ Exp)
noloopOpToPre 𝜓 pis =
  let chOp 𝒪 {oP = p, o𝑛 = 𝑛} =
        let b = 𝜓 M.! p M.! 𝑛
         in IfElse b (1 #) (0 #)
      pis' = (M.elems . M.map (M.map (M.map ((...+) . map chOp)))) pis
   in M.unionsWith (M.unionWith (:+)) pis'
