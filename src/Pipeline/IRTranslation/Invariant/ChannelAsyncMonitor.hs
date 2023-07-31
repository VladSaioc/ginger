module Pipeline.IRTranslation.Invariant.ChannelAsyncMonitor (asyncChannelMonitors) where

import Backend.Ast
import Backend.Utilities
import Data.List qualified as L
import Data.Map qualified as M
import Data.Maybe qualified as Mb
import IR.Utilities
import Pipeline.IRTranslation.Meta.Channel
import Pipeline.IRTranslation.Meta.Loop
import Pipeline.IRTranslation.Utilities

{- Retrieves all asynchronous channel monitor expressions by analyzing
all loop and non-loop channel oeprations. The produced expressions
represent a relationship between process progress and their impact
on the size of the channel buffer.

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
asyncChannelMonitors :: P ↦ (𝐶 ↦ 𝒪s) -> [ℒ] -> 𝐶 ↦ Exp
asyncChannelMonitors noloopOps ls =
  let noloopSubexps = L.map snd (M.toList (M.map noloopMonitors noloopOps))
      loopSubexps = L.map loopMonitor ls
      subexps = M.unionsWith (M.unionWith (:+)) (noloopSubexps ++ loopSubexps)
      chanMonitor dir =
        let sendops = Mb.fromMaybe (0 #) (M.lookup S dir)
            recvops = Mb.fromMaybe (0 #) (M.lookup R dir)
         in sendops :- recvops
   in M.map chanMonitor subexps

{- Monitor asynchronous channel buffer length by analyzing the operations in a loop.
It returns an expression representing the resource contribution of each channel
operated on in a loop.
Depends on: ℓ, with the following properties:
1. π(ℓ) is the process id counter variable of the loop
2. op(ℓ) = {(n₁, c₁{!,?}), ..., (nₘ, cₘ{!,?})} are loop channel operations.
3. lo(ℓ) is the lower bound expression
4. x(ℓ) is the loop index variable
5. exit(ℓ) is the exit point
6. b(ℓ) is the reachability condition for the loop.

Produces:
[ c ↦ [
  ! ↦ if b then
          (x(ℓ) - lo(ℓ)) * |{ c! | (n, c!) ∈ op(ℓ) }|
        + (Σ ∀(n, c!) ∈ op(ℓ).
            if n < π(ℓ) < exit(ℓ) then 1 else 0)
      else 0,
  ? ↦ if b then
          (x(ℓ) - lo(ℓ)) * |{ c? | (n, c?) ∈ op(ℓ) }|
        + (Σ ∀(n, c?) ∈ op(ℓ).
            if n < π(ℓ) < exit(ℓ) then 1 else 0)
      else 0 ]
  | ∀ c, (n, cd) ∈ op(ℓ) ]
-}
loopMonitor :: ℒ -> 𝐶 ↦ (OpDir ↦ Exp)
loopMonitor (ℒ {l𝑋 = var, lower, lExit = exit, l𝒪s = chans}) =
  let x = (var @)
      singleOp ch =
        let 𝒪 {o𝑛 = 𝑛, oP = pid} = ch
            pc = π pid
            hasPassedOp = ((𝑛 #) :< pc) :&& (pc :< (exit #))
         in IfElse hasPassedOp (1 #) (0 #)
      chanSubexp ops =
        let iterations = (x :- lower) :* (length ops #)
            ops' = L.map singleOp ops
         in iterations :+ (ops' ...+)
   in M.map (M.map chanSubexp) chans

{- Organize and compose under addition all non-loop monitor
sub-expressions for every asynchronous channel for a given process.
Depends on: π, ϕ

Produces:
⋃ ∀ c.
  [c ↦ [
    ! ↦ {if n < pc(π) then 1 else 0) | ∀(n, c!) ∈ ϕ(π) },
    ? ↦ {if n < pc(π) then 1 else 0) | ∀(n, c!) ∈ ϕ(π) }
  ]]
-}
noloopMonitors :: 𝐶 ↦ 𝒪s -> 𝐶 ↦ (OpDir ↦ Exp)
noloopMonitors =
  let subexps = L.map noloopMonitor
      setTransform = (...+) . subexps
   in M.map (M.map setTransform)

{- Monitor sub-expression for a non-loop single asynchronous channel operation.
If the operation has occurred, its resource contribution is 1.
Depends on: π, n, where n ∈ dom(Π(π)), b (reachability condition)

if b then if n < pc(π) then 1 else 0 else 0
-}
noloopMonitor :: 𝒪 -> Exp
noloopMonitor ch =
  let 𝒪 {oP = p, o𝑛 = n, oPathexp = b} = ch
      pc = π p
      passed = (n #) :< pc
   in IfElse b (IfElse passed (1 #) (0 #)) (0 #)
