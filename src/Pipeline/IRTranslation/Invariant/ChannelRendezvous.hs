module Pipeline.IRTranslation.Invariant.ChannelRendezvous where

import Backend.Ast
import Backend.Utilities
import Data.List qualified as L
import Data.Map qualified as M
import Data.Maybe qualified as Mb
import Data.Set qualified as S
import IR.Utilities
import Pipeline.IRTranslation.Utilities

asyncNoRendezvous :: KEnv -> PChInsns -> [Loop] -> [Exp]
asyncNoRendezvous kenv atomicOps ls =
  let atomicRvC pid = M.mapWithKey (chanopsToRendezvous kenv pid)
      atomicRvs = concat $ concatMap M.elems (M.mapWithKey atomicRvC atomicOps)
      loopRvs = concatMap (loopToNoRendezvous kenv) ls
   in atomicRvs ++ loopRvs

loopToNoRendezvous :: KEnv -> Loop -> [Exp]
loopToNoRendezvous kenv Loop {pid, chans} =
  let invs = M.mapWithKey (chanopsToRendezvous kenv pid) chans
   in concat $ M.elems invs

chanopsToRendezvous :: KEnv -> Pid -> String -> ChOps -> [Exp]
chanopsToRendezvous kenv pid c =
  let sends = S.toList . Mb.fromMaybe S.empty . M.lookup S
   in L.map (sendToNoRendezvous kenv pid c) . sends

{- Creates an invariant sub-expression stipulating that the program
counter will never reach rendezvous points if the channel is buffered.
Depends on: κ, ϕ, π

Produces:
0 < κ(c) => pc(π) /= n + 1
-}
sendToNoRendezvous :: KEnv -> Pid -> String -> PCounter -> Exp
sendToNoRendezvous kenv pid c n =
  let pc = π pid
      k = Mb.fromMaybe (0 #) (M.lookup c kenv)
   in Implies (Lt (0 #) k) (Ne pc ((n + 1) #))
