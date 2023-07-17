module Pipeline.IRTranslation.Invariant.ChannelSyncMonitor (syncChannelMonitors) where

import Backend.Ast
import Backend.Utilities
import Data.List qualified as L
import Data.Map qualified as M
import Data.Maybe qualified as Mb
import Data.Set qualified as S
import IR.Utilities
import Pipeline.IRTranslation.Utilities

{- Retrieves all synchronous channel monitor expressions by analyzing
all loop and non-loop channel oeprations. The produced expressions
represent a relationship between process progress and the number of
synchronization that should have already occurred.

Depends on:
1. All program loops: [ℓ]
2. All non-loop operations:
    O = {(π, n, o) | (n, o) ∉ op(ℓ), ℓ ∈ [ℓ], (n, o) ∈ ϕ, (π, ϕ) ∈ Π }

Produces:
[ c ↦ e1 - e2 | ∀ c. (n, cd) ∈ ϕ, (π, ϕ) ∈ Π,
    e1 =  Σ ∀ ℓ, (c, [! ↦ e']) ∈ loopMonitor(ℓ). e'
        + Σ (π, n, !) ∈ O, e' = noloopMonitor(π, n). e',
    e2 =  Σ ∀ ℓ, (c, [? ↦ e']) ∈ loopMonitor(ℓ). e'
        + Σ (π, n, ?) ∈ O, e' = noloopMonitor(π, n). e' ]
-}
syncChannelMonitors :: PChInsns -> [Loop] -> ChMap Exp
syncChannelMonitors noloopOps ls =
  let noloopSubexps = L.map snd (M.toList (M.mapWithKey noloopMonitors noloopOps))
      loopSubexps = L.map loopMonitor ls
      subexps = M.unionsWith (M.unionWith Plus) (noloopSubexps ++ loopSubexps)
      chanMonitor dir =
        let sendops = Mb.fromMaybe (0 #) (M.lookup S dir)
            recvops = Mb.fromMaybe (0 #) (M.lookup R dir)
         in Minus sendops recvops
   in M.map chanMonitor subexps

{- Monitor synchronous channel progress by analyzing the operations in a loop.
It returns an expression representing the resource contribution of each channel
operated on in a loop.
Depends on: ℓ, with the following properties:
1. π(ℓ) is the process id counter variable of the loop
2. op(ℓ) = {(n₁, c₁{!,?}), ..., (nₘ, cₘ{!,?})} are loop channel operations.
3. lo(ℓ) is the lower bound expression
4. x(ℓ) is the loop index variable
5. exit(ℓ) is the exit point

Produces:
[ c ↦ [
  ! ↦ 2(x(ℓ) - lo(ℓ)) * |{ c! | (n, c!) ∈ op(ℓ) }|
    + (Σ ∀(n, c!) ∈ op(ℓ).
      if n < π(ℓ) < exit(ℓ) then 1 else 0
    + if n + 1 < π(ℓ) < exit(ℓ) then 1 else 0),
  ? ↦ 2(x(ℓ) - lo(ℓ)) * |{ c? | (n, c?) ∈ op(ℓ) }|
    - (Σ ∀(n, c?) ∈ op(ℓ).
      if n < π(ℓ) < exit(ℓ) then 2 else 0) ]
  | ∀ c, (n, cd) ∈ op(ℓ) ]
-}
loopMonitor :: Loop -> ChMap (M.Map OpDir Exp)
loopMonitor (Loop {pid, var, lower, exitP, chans}) =
  let x = (var @)
      pc = π pid
      sendOp n =
        let synced = And (Lt (n #) pc) (Lt pc (exitP #))
            rendezvous = And (Lt ((n + 1) #) pc) (Lt pc (exitP #))
         in Plus
              (IfElse synced (1 #) (0 #))
              (IfElse rendezvous (1 #) (0 #))
      receiveOp n =
        let synced = And (Lt (n #) pc) (Lt pc (exitP #))
         in IfElse synced (2 #) (0 #)
      chanSubexp d ops =
        let singleOp = case d of
              S -> sendOp
              R -> receiveOp
            iterations = Mult (Minus x lower) (S.size ops #)
            x2 = Mult (2 #) iterations
            ops' = L.map singleOp (S.toList ops)
         in Plus x2 (ops' ...+)
   in M.map (M.mapWithKey chanSubexp) chans

{- Organize and compose under addition all non-loop monitor
sub-expressions for every synchronous channel for a given process.
Depends on: π, ϕ

Produces:
[c ↦ [
  ! ↦ {(if n < pc(π) then 1 else 0) + (if n + 1 < pc(π) then 1 else 0) | ∀(n, c!) ∈ ϕ },
  ? ↦ {if n < pc(π) then 2 else 0 | ∀(n, c?) ∈ ϕ }]
  | ∀ c, (n, cd) ∈ ϕ ]
-}
noloopMonitors :: Pid -> ChMap ChOps -> ChMap (M.Map OpDir Exp)
noloopMonitors pid =
  let pc = π pid
      noloopMonitor d = L.map $ case d of
        S -> sendNoloopMonitor pc
        R -> receiveNoloopMonitor pc
      subexps = M.map (...+) . M.mapWithKey noloopMonitor . M.map S.toList
   in M.map subexps

{- Monitor sub-expression for a non-loop single synchronous channel send.
After synchronization, its resource contribution is 1. After the rendezvous,
its resource contribution is 1 more.
Depends on: π, n, where n ∈ dom(Π(π))

  if n < pc(π) then 1 else 0
+ if n + 1 < pc(π) then 1 else 0
-}
sendNoloopMonitor :: Exp -> PCounter -> Exp
sendNoloopMonitor pc n =
  let synced = Lt (n #) pc
      rendezvous = Lt ((n + 1) #) pc
   in Plus
        (IfElse synced (1 #) (0 #))
        (IfElse rendezvous (1 #) (0 #))

{- Monitor sub-expression for a non-loop single synchronous channel receive.
After synchronization, its resource contribution is 2.
Depends on: π ∈ dom(Π), n ∈ dom(Π(π))

if n < pc(π) then 2 else 0
-}
receiveNoloopMonitor :: Exp -> PCounter -> Exp
receiveNoloopMonitor pc n =
  let synced = Lt (n #) pc
   in IfElse synced (2 #) (0 #)
