module Pipeline.IRTranslation.CommPrecondition (preconditions) where

import Backend.Ast
import Backend.Utilities
import Data.List qualified as L
import Data.Map qualified as M
import Data.Maybe qualified as Mb
import Data.Set qualified as S
import IR.Utilities
import Pipeline.IRTranslation.Utilities

iterations :: Exp -> Exp -> Exp
iterations lo hi = Call "iterations" [lo, hi]

preconditions :: KEnv -> PChInsns -> [Loop] -> [Exp]
preconditions kenv noloops loops =
  let plus e e' = ([e, e'] .+.)
      lR = M.unionsWith (M.unionWith plus) (L.map loopToResource loops)
      nR = noloopOpsToResource noloops
      cs = M.keys lR ++ M.keys nR
      prc c =
        let k = Mb.fromMaybe (0 #) (M.lookup c kenv)
            cR r = Mb.fromMaybe M.empty (M.lookup c r)
            cdR d r = Mb.fromMaybe (0 #) (M.lookup d r)
            (clR, cnR) = (cR lR, cR nR)

            sends = plus (cdR S clR) (cdR S cnR)
            rcvs = plus (cdR R clR) (cdR R cnR)

            rcvsUnblock = Leq rcvs sends
            sndsUnblock = Leq sends (plus rcvs k)
         in And rcvsUnblock sndsUnblock
   in L.map prc cs

{- Constructs the resource contribution resulting from
loop channel operations.
Depends on: ℓ = (x, e, e', o!, o?)

Produces:
∀ c. |o!(c)| * iterations(e, e')
∀ c. |o?(c)| * iterations(e, e')
-}
loopToResource :: Loop -> ChMap (M.Map OpDir Exp)
loopToResource (Loop {lower, upper, chans}) =
  let iter ops = case S.size ops of
        0 -> (0 #)
        1 -> iterations lower upper
        n -> Mult (n #) (iterations lower upper)
   in M.map (M.map iter) chans

{- Constructs the resource contribution resulting from
non-loop channel operations.
Depends on: c

Produces:
∀ c. 𝚺 π ∈ Π. |o!(c, π)|, |o?(c, π)|
-}
noloopOpsToResource :: PChInsns -> ChMap (M.Map OpDir Exp)
noloopOpsToResource pis =
  let pis' = M.map (M.map (M.map S.size)) pis
      pis'' = M.unionsWith (M.unionWith (+)) pis'
   in M.map (M.map (#)) pis''
