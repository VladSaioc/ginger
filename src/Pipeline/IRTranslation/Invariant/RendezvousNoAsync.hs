module Pipeline.IRTranslation.Invariant.RendezvousNoAsync (noAsyncRendezvous) where

import Backend.Ast
import Backend.Utilities
import Data.List qualified as L
import Data.Map qualified as M
import Data.Maybe qualified as Mb
import IR.Utilities
import Pipeline.IRTranslation.Meta.Channel
import Pipeline.IRTranslation.Meta.Loop
import Pipeline.IRTranslation.Utilities

{- For every channel send operation of every process, adds
an invariant clause stipulating that the process never reaches
the rendezvous instruction if the channel is buffered.
-}
noAsyncRendezvous :: K -> P ↦ (𝐶 ↦ 𝒪s) -> [ℒ] -> [Exp]
noAsyncRendezvous 𝜅 os ls =
  let atomicRvC = M.map (chanopsToRendezvous 𝜅)
      atomicRvs = concat $ concatMap M.elems (M.map atomicRvC os)
      loopRvs = concatMap (loopToNoRendezvous 𝜅) ls
   in atomicRvs ++ loopRvs

loopToNoRendezvous :: K -> ℒ -> [Exp]
loopToNoRendezvous κ ℒ {l𝒪s} =
  let invs = M.map (chanopsToRendezvous κ) l𝒪s
   in concat $ M.elems invs

chanopsToRendezvous :: K -> 𝒪s -> [Exp]
chanopsToRendezvous κ =
  let sends = Mb.fromMaybe [] . M.lookup S
   in L.map (sendToNoRendezvous κ) . sends

{- Creates an invariant sub-expression stipulating that the program
counter will never reach rendezvous points if the channel is buffered.
Depends on: κ, 𝜙, π

Produces:
0 < κ(c) => pc(π) != n + 1
-}
sendToNoRendezvous :: K -> 𝒪 -> Exp
sendToNoRendezvous κ 𝒪 {oP = pid, o𝐶 = c, o𝑛 = 𝑛} =
  let pc = π pid
      k = Mb.fromMaybe (0 #) (M.lookup c κ)
   in ((0 #) :< k) :==> (pc :!= ((𝑛 + 1) #))
