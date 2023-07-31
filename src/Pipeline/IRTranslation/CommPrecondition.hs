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

iterations :: Exp -> Exp -> Exp
iterations lo hi = Call "iter" [lo, hi]

preconditions :: K -> P ↦ (𝐶 ↦ 𝒪s) -> [ℒ] -> [Exp]
preconditions κ noloops loops =
  let plus e e' = ([e, e'] ...+)
      lR = M.unionsWith (M.unionWith plus) (L.map loopToPre loops)
      nR = noloopOpToPre noloops
      cs = M.keys lR ++ M.keys nR
      prc c =
        let k = Mb.fromJust (M.lookup c κ)
            cR r = Mb.fromMaybe M.empty (M.lookup c r)
            cdR d r = Mb.fromMaybe (0 #) (M.lookup d r)
            (clR, cnR) = (cR lR, cR nR)

            sends = plus (cdR S clR) (cdR S cnR)
            rcvs = plus (cdR R clR) (cdR R cnR)

            rcvsUnblock = rcvs :<= sends
            sndsUnblock = sends :<= plus rcvs k
         in rcvsUnblock :&& sndsUnblock
   in L.map prc cs

{- Constructs the resource contribution resulting from
loop channel operations.
Depends on: ℓ = (x, e, e', o!, o?)

Produces:
∀ c. |o!(c)| * iterations(e, e')
∀ c. |o?(c)| * iterations(e, e')
-}
loopToPre :: ℒ -> 𝐶 ↦ (OpDir ↦ Exp)
loopToPre (ℒ {lower, upper, l𝒪s = os, lPathexp = b}) =
  let iter ops =
        let e = case length ops of
              0 -> (0 #)
              1 -> iterations lower upper
              n -> (n #) :* iterations lower upper
         in IfElse b e (0 #)
   in M.map (M.map iter) os

{- Constructs the resource contribution resulting from
non-loop channel operations.
Depends on: c

Produces:
∀ c. 𝚺 π ∈ Π. |o!(c, π)|, |o?(c, π)|
-}
noloopOpToPre :: P ↦ (𝐶 ↦ 𝒪s) -> 𝐶 ↦ (OpDir ↦ Exp)
noloopOpToPre pis =
  let chOp 𝒪 {oPathexp = b} = IfElse b (1 #) (0 #)
      pis' = (M.elems . M.map (M.map (M.map ((...+) . map chOp)))) pis
   in M.unionsWith (M.unionWith (:+)) pis'
