module Pipeline.IRTranslation.Invariant.ChannelRendezvous where

import Backend.Ast
import Backend.Utilities
import Data.List qualified as L
import Data.Map qualified as M
import Data.Maybe qualified as Mb
import IR.Utilities
import Pipeline.IRTranslation.Utilities

asyncNoRendezvous :: KEnv -> PChInsns -> [Loop] -> [Exp]
asyncNoRendezvous kenv atomicOps ls =
  let atomicRvC = M.map (chanopsToRendezvous kenv)
      atomicRvs = concat $ concatMap M.elems (M.map atomicRvC atomicOps)
      loopRvs = concatMap (loopToNoRendezvous kenv) ls
   in atomicRvs ++ loopRvs

loopToNoRendezvous :: KEnv -> Loop -> [Exp]
loopToNoRendezvous kenv Loop {chans} =
  let invs = M.map (chanopsToRendezvous kenv) chans
   in concat $ M.elems invs

chanopsToRendezvous :: KEnv -> ChOps -> [Exp]
chanopsToRendezvous kenv =
  let sends = Mb.fromMaybe [] . M.lookup S
   in L.map (sendToNoRendezvous kenv) . sends

{- Creates an invariant sub-expression stipulating that the program
counter will never reach rendezvous points if the channel is buffered.
Depends on: κ, ϕ, π

Produces:
0 < κ(c) => pc(π) /= n + 1
-}
sendToNoRendezvous :: KEnv -> ChannelMeta -> Exp
sendToNoRendezvous kenv ch =
  let ChannelMeta {cmPid = pid, cmVar = c, cmPoint = n} = ch
      pc = π pid
      k = Mb.fromMaybe (0 #) (M.lookup c kenv)
   in Implies (Lt (0 #) k) (Ne pc ((n + 1) #))
