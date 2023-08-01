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
    O = {(π, 𝑛, o) | (𝑛, o) ∉ op(ℓ), ℓ ∈ [ℓ], (𝑛, o) ∈ ϕ, (π, ϕ) ∈ Π }

Produces:
[ c ↦ e1 - e2 | ∀ c. (𝑛, cd) ∈ ϕ, (π, ϕ) ∈ Π,
    e1 =  Σ ∀ ℓ, (c, [! ↦ e']) ∈ loopMonitor(ℓ). e'
        + Σ (π, 𝑛, !) ∈ O, e' = noloopMonitor(π, 𝑛). e',
    e2 =  Σ ∀ ℓ, (c, [? ↦ e']) ∈ loopMonitor(ℓ). e'
        + Σ (π, 𝑛, ?) ∈ O, e' = noloopMonitor(π, 𝑛). e' ]
-}
syncChannelMonitors :: P ↦ (𝐶 ↦ 𝒪s) -> [ℒ] -> 𝐶 ↦ Exp
syncChannelMonitors noloopOps ls =
  let noloopSubexps = L.map snd (M.toList (M.map noloopMonitors noloopOps))
      loopSubexps = L.map loopMonitor ls
      subexps = M.unionsWith (M.unionWith (:+)) (noloopSubexps ++ loopSubexps)
      chanMonitor dir =
        let sendops = Mb.fromMaybe (0 #) (M.lookup S dir)
            recvops = Mb.fromMaybe (0 #) (M.lookup R dir)
         in sendops :- recvops
   in M.map chanMonitor subexps

{- Monitor synchronous channel progress by analyzing the operations in a loop.
It returns an expression representing the resource contribution of each channel
operated on in a loop.
Depends on: ℓ, with the following properties:
1. π(ℓ) is the process id counter variable of the loop
2. op(ℓ) = {(𝑛₁, c₁{!,?}), ..., (𝑛ₘ, cₘ{!,?})} are loop channel operations.
3. lo(ℓ) is the lower bound expression
4. x(ℓ) is the loop index variable
5. exit(ℓ) is the exit point
6. b(ℓ) is the loop reachability condition

Produces:
[ c ↦ [
  ! ↦ if b(ℓ) then
          2(x(ℓ) - lo(ℓ)) * |{ c! | (𝑛, c!) ∈ op(ℓ) }|
        + (Σ ∀(𝑛, c!) ∈ op(ℓ).
            if 𝑛 < π(ℓ) < exit(ℓ) then 1 else 0
          + if 𝑛 + 1 < π(ℓ) < exit(ℓ) then 1 else 0)
      else 0,
  ? ↦ if b(ℓ) then
          2(x(ℓ) - lo(ℓ)) * |{ c? | (𝑛, c?) ∈ op(ℓ) }|
        + (Σ ∀(𝑛, c?) ∈ op(ℓ).
            if 𝑛 < π(ℓ) < exit(ℓ) then 2 else 0)
      else 0 ]
  | ∀ c, (𝑛, cd) ∈ op(ℓ) ]
-}
loopMonitor :: ℒ -> 𝐶 ↦ (OpDir ↦ Exp)
loopMonitor (ℒ {lP = p, l𝑋 = var, lower, lExit = 𝑛', l𝒪s = chans, lPathexp = b}) =
  let x = (var @)
      pc = π p
      ext = (𝑛' #)
      singleOp 𝒪 {oDir = d, o𝑛 = 𝑛} =
        let synced = ((𝑛 #) :< pc) :&& (pc :< ext)
         in case d of
              S ->
                let rendezvous = (((𝑛 + 1) #) :< pc) :&& (pc :< ext)
                 in IfElse synced (1 #) (0 #) :+ IfElse rendezvous (1 #) (0 #)
              R -> IfElse synced (2 #) (0 #)
      chanSubexp ops =
        let iterations = (x :- lower) :* (length ops #)
            x2 = ((2 #) :* iterations)
            ops' = L.map singleOp ops
         in IfElse b (x2 :+ (ops' ...+)) (0 #)
   in M.map (M.map chanSubexp) chans

{- Organize and compose under addition all non-loop monitor
sub-expressions for every synchronous channel for a given process.
Depends on: π, ϕ

Produces:
[c ↦ [
  ! ↦ {if b(𝑛) then
          if 𝑛 < pc(π) then 1 else 0) + (if 𝑛 + 1 < pc(π) then 1 else 0)
        else 0 | ∀(𝑛, c!) ∈ ϕ },
  ? ↦ {if 𝑛 < pc(π) then 2 else 0 | ∀(𝑛, c?) ∈ ϕ }]
  | ∀ c, (𝑛, cd) ∈ ϕ ]
-}
noloopMonitors :: 𝐶 ↦ 𝒪s -> 𝐶 ↦ (OpDir ↦ Exp)
noloopMonitors = M.map (M.map ((...+) . map noloopMonitor))

{- Monitor sub-expression for a non-loop single synchronous channel operation.

For send operations:
After synchronization, its resource contribution is 1. After the rendezvous,
its resource contribution is 1 more.
For receive operations:
After synchronization, its resource contribution is 2.

Depends on: π, 𝑛, where 𝑛 ∈ dom(Π(π))

Depnding on the operation direction, it produces:
  ! ↦ if b then
          if 𝑛 < pc(π) then 1 else 0
        + if 𝑛 + 1 < pc(π) then 1 else 0
      else 0

  ? ↦ if b then
          if 𝑛 < pc(π) then 2 else 0 else
      else 0
-}
noloopMonitor :: 𝒪 -> Exp
noloopMonitor 𝒪 {oP = pid, oDir = d, o𝑛 = 𝑛, oPathexp = b} =
  let pc = π pid
      synced = (𝑛 #) :< pc
      rendezvous = ((𝑛 + 1) #) :< pc
      monitor = case d of
        S -> IfElse (b :&& synced) (1 #) (0 #) :+ IfElse (b :&& rendezvous) (1 #) (0 #)
        R -> IfElse (b :&& synced) (2 #) (0 #)
   in monitor
