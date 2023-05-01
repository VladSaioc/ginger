module Pipeline.IRTranslation.Invariant.ChannelMonitor (channelMonitors) where

import Backend.Ast
import Backend.Utilities
import Data.List qualified as L
import Data.Map qualified as M
import Data.Maybe qualified as Mb
import Data.Set qualified as S
import IR.Utilities
import Pipeline.IRTranslation.Utilities

channelMonitors :: PChInsns -> [Loop] -> [Exp]
channelMonitors noloopOps ls =
  let noloopSubexps = L.map snd (M.toList (M.mapWithKey noloopMonitors noloopOps))
      loopSubexps = L.map loopMonitor ls
      subexps = M.unionsWith (M.unionWith Plus) (noloopSubexps ++ loopSubexps)
      chanMonitor c dir =
        let sendops = Mb.fromMaybe (0 #) (M.lookup S dir)
            recvops = Mb.fromMaybe (0 #) (M.lookup R dir)
         in Eq (c @) (Minus sendops recvops)
   in M.elems (M.mapWithKey chanMonitor subexps)

{- Monitor channel value by analyzing a loop
Depends on:
-}
loopMonitor :: Loop -> ChMap (M.Map OpDir Exp)
loopMonitor (Loop {pid, var, lower, exitP, chans}) =
  let x = (var @)
      pc = ((pid <|) @)
      singleOp op =
        let hasPassedOp = And (Lt (op #) pc) (Lt pc (exitP #))
         in IfElse hasPassedOp (1 #) (0 #)
      chanSubexp ops =
        let iterations = Mult (Minus x lower) (S.size ops #)
            ops' = L.map singleOp (S.toList ops)
         in Plus iterations (ops' .+.)
   in M.map (M.map chanSubexp) chans

{- Organize and compose under addition all non-loop monitor
sub-expressions for every channel for a given process.
Depends on: π,

-}
noloopMonitors :: Pid -> ChMap ChOps -> ChMap (M.Map OpDir Exp)
noloopMonitors pid =
  let pc = ((pid <|) @)
      subexps = L.map (noloopMonitor pc) . S.toList
      setTransform = (.+.) . subexps
   in M.map (M.map setTransform)

{- Monitor sub-expression for a non-loop single channel operation. If the
operation has occurred, its resource contribution is 1.
Depends on: π, n, where n ∈ dom(Π(π))

if n < pc(π) then 1 else 0
-}
noloopMonitor :: Exp -> PCounter -> Exp
noloopMonitor pc n =
  let passed = Lt (n #) pc
   in IfElse passed (1 #) (0 #)