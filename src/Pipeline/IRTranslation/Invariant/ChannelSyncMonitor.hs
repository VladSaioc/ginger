module Pipeline.IRTranslation.Invariant.ChannelSyncMonitor (syncChannelMonitors) where

import Backend.Ast
import Backend.Utilities
import Data.List qualified as L
import Data.Map qualified as M
import Data.Maybe qualified as Mb
import IR.Utilities
import Pipeline.IRTranslation.Meta.Channel
import Pipeline.IRTranslation.Meta.Loop
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
syncChannelMonitors :: P ↦ (𝐶 ↦ 𝒪s) -> [ℒ] -> 𝐶 ↦ Exp
syncChannelMonitors noloopOps ls =
  let noloopSubexps = L.map snd (M.toList (M.map noloopMonitors noloopOps))
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
6. b(ℓ) is the loop reachability condition

Produces:
[ c ↦ [
  ! ↦ if b(ℓ) then
          2(x(ℓ) - lo(ℓ)) * |{ c! | (n, c!) ∈ op(ℓ) }|
        + (Σ ∀(n, c!) ∈ op(ℓ).
            if n < π(ℓ) < exit(ℓ) then 1 else 0
          + if n + 1 < π(ℓ) < exit(ℓ) then 1 else 0)
      else 0,
  ? ↦ if b(ℓ) then
          2(x(ℓ) - lo(ℓ)) * |{ c? | (n, c?) ∈ op(ℓ) }|
        + (Σ ∀(n, c?) ∈ op(ℓ).
            if n < π(ℓ) < exit(ℓ) then 2 else 0)
      else 0 ]
  | ∀ c, (n, cd) ∈ op(ℓ) ]
-}
loopMonitor :: ℒ -> 𝐶 ↦ (OpDir ↦ Exp)
loopMonitor (ℒ {lP = p, l𝑋 = var, lower, lExit = exit, l𝒪s = chans, lPathexp = b}) =
  let x = (var @)
      pc = π p
      ext = (exit #)
      singleOp 𝒪 {oDir = d, o𝑛 = 𝑛} =
        let synced = Lt (𝑛 #) pc :&& Lt pc ext
         in case d of
              S ->
                let rendezvous = Lt ((𝑛 + 1) #) pc :&& Lt pc ext
                 in Plus
                      (IfElse synced (1 #) (0 #))
                      (IfElse rendezvous (1 #) (0 #))
              R -> IfElse synced (2 #) (0 #)
      chanSubexp ops =
        let iterations = Mult (Minus x lower) (length ops #)
            x2 = Mult (2 #) iterations
            ops' = L.map singleOp ops
         in IfElse b (Plus x2 (ops' ...+)) (0 #)
   in M.map (M.map chanSubexp) chans

{- Organize and compose under addition all non-loop monitor
sub-expressions for every synchronous channel for a given process.
Depends on: π, ϕ

Produces:
[c ↦ [
  ! ↦ {if b(n) then
          if n < pc(π) then 1 else 0) + (if n + 1 < pc(π) then 1 else 0)
        else 0 | ∀(n, c!) ∈ ϕ },
  ? ↦ {if n < pc(π) then 2 else 0 | ∀(n, c?) ∈ ϕ }]
  | ∀ c, (n, cd) ∈ ϕ ]
-}
noloopMonitors :: 𝐶 ↦ 𝒪s -> 𝐶 ↦ (OpDir ↦ Exp)
noloopMonitors = M.map (M.map ((...+) . map noloopMonitor))

{- Monitor sub-expression for a non-loop single synchronous channel operation.

For send operations:
After synchronization, its resource contribution is 1. After the rendezvous,
its resource contribution is 1 more.
For receive operations:
After synchronization, its resource contribution is 2.

Depends on: π, n, where n ∈ dom(Π(π))

Depnding on the operation direction, it produces:
  ! ↦ if b then
          if n < pc(π) then 1 else 0
        + if n + 1 < pc(π) then 1 else 0
      else 0

  ? ↦ if b then
          if n < pc(π) then 2 else 0 else
      else 0
-}
noloopMonitor :: 𝒪 -> Exp
noloopMonitor 𝒪 {oP = pid, oDir = d, o𝑛 = 𝑛, oPathexp = b} =
  let pc = π pid
      synced = Lt (𝑛 #) pc
      rendezvous = Lt ((𝑛 + 1) #) pc
      monitor = case d of
        S ->
          Plus
            (IfElse synced (1 #) (0 #))
            (IfElse rendezvous (1 #) (0 #))
        R -> IfElse synced (2 #) (0 #)
   in IfElse b monitor (0 #)
