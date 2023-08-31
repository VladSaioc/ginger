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
import Utilities.Collection

{- | Retrieves all synchronous channel monitor expressions by analyzing
all loop and non-loop channel oeprations. The produced expressions
represent a relationship between process progress and the number of
synchronization that should have already occurred.
Depends on:

1. Reachability conditions for all processes:
    𝜓 = [π ↦ [𝑛 ↦ e | 𝑛 ∈ dom(𝛱(π))] | π ∈ dom(𝛱)]𝛱)]
2. All program loops: [ℓ]
3. All non-loop operations:
    O = {(π, 𝑛, o) | (𝑛, o) ∉ op(ℓ), ℓ ∈ [ℓ], (𝑛, o) ∈ 𝜙, (π, 𝜙) ∈ 𝛱 }

Produces:

> [ c ↦ e1 - e2 | ∀ c. (𝑛, cd) ∈ 𝜙, (π, 𝜙) ∈ 𝛱,
>     e1 =  𝛴 ∀ ℓ, (c, [! ↦ e']) ∈ loopMonitor(ℓ). e'
>         + 𝛴 (π, 𝑛, !) ∈ O, e' = noloopMonitor(π, 𝑛). e',
>     e2 =  𝛴 ∀ ℓ, (c, [? ↦ e']) ∈ loopMonitor(ℓ). e'
>         + 𝛴 (π, 𝑛, ?) ∈ O, e' = noloopMonitor(π, 𝑛). e' ]
-}
syncChannelMonitors :: 𝛹 -> P ↦ (𝐶 ↦ 𝒪s) -> [ℒ] -> 𝐶 ↦ Exp
syncChannelMonitors 𝜓 noloopOps ls =
  let noloopSubexps = L.map snd (M.toList (M.map (noloopMonitors 𝜓) noloopOps))
      loopSubexps = L.map (loopMonitor 𝜓) ls
      subexps = M.unionsWith (M.unionWith (:+)) (noloopSubexps ++ loopSubexps)
      chanMonitor dir =
        let sendops = Mb.fromMaybe (0 #) (M.lookup S dir)
            recvops = Mb.fromMaybe (0 #) (M.lookup R dir)
         in sendops :- recvops
   in M.map chanMonitor subexps

{- | Monitor synchronous channel progress by analyzing the operations in a loop.
It returns an expression representing the resource contribution of each channel
operated on in a loop.
Depends on:

I. Reachability conditions for all processes:
    𝜓 = [π ↦ [𝑛 ↦ e | 𝑛 ∈ dom(𝛱(π))] | π ∈ dom(𝛱)]

II. ℓ = (π, O, x, e₁, e₂, 𝑛₀, 𝑛'), with the following properties:
1. π is the process id of the loop
2. O = {(𝑛₁, c₁{!,?}), ..., (𝑛ₘ, cₘ{!,?})} are loop channel operations.
3. x is the loop index variable
4. e₁ is the lower bound expression
5. e₂ is the upper bound expression
6. 𝑛 is the guard point
7. 𝑛' is the exit point

Produces:

> [ c ↦ [
>   ! ↦ if 𝜓(π)(𝑛₀) then
>           2(x - e₁) * |{ c! | (𝑛, c!) ∈ O }|
>         + (𝛴 ∀(𝑛, c!) ∈ O.
>             if 𝑛 < pc(π) < 𝑛' then 1 else 0
>           + if 𝑛 + 1 < pc(π) < 𝑛' then 1 else 0)
>       else 0,
>   ? ↦ if 𝜓(π)(𝑛₀) then
>           2(x - e₁) * |{ c? | (𝑛, c?) ∈ O }|
>         + (𝛴 ∀(𝑛, c?) ∈ O.
>             if 𝑛 < pc(π) < 𝑛' then 2 else 0)
>       else 0 ]
>   | ∀ c, (𝑛, cd) ∈ O ]
-}
loopMonitor :: 𝛹 -> ℒ -> 𝐶 ↦ (OpDir ↦ Exp)
loopMonitor 𝜓 (ℒ {lP = p, l𝑋 = var, lower, l𝑛 = 𝑛, lExit = 𝑛', l𝒪s = chans}) =
  let b = 𝜓 M.! p M.! 𝑛
      x = (var @)
      pc = π p
      ext = (𝑛' #)
      singleOp 𝒪 {oDir = d, o𝑛 = 𝑛ᵢ} =
        let synced = ((𝑛ᵢ #) :< pc) :&& (pc :< ext)
         in case d of
              S ->
                let rendezvous = (((𝑛ᵢ + 1) #) :< pc) :&& (pc :< ext)
                 in IfElse synced (1 #) (0 #) :+ IfElse rendezvous (1 #) (0 #)
              R -> IfElse synced (2 #) (0 #)
      chanSubexp ops =
        let iterations = (x :- lower) :* (length ops #)
            x2 = ((2 #) :* iterations)
            ops' = L.map singleOp ops
         in IfElse b (x2 :+ (ops' ...+)) (0 #)
   in M.map (M.map chanSubexp) chans

{- | Organize and compose under addition all non-loop monitor
sub-expressions for every synchronous channel for a given process.
Depends on: π, 𝜙

Reachability conditions for all processes:
  𝜓 = [π ↦ [𝑛 ↦ e | 𝑛 ∈ dom(𝛱(π))] | π ∈ dom(𝛱)]

Produces:

> [c ↦ [
>   ! ↦ { (if 𝜓(π)(𝑛) && 𝑛 < pc(π) then 1 else 0)
>       + (if 𝑛 + 1 < pc(π) then 1 else 0) | ∀(𝑛, c!) ∈ 𝜙 },
>   ? ↦ {if 𝜓(π)(𝑛) && 𝑛 < pc(π) then 2 else 0 | ∀(𝑛, c?) ∈ 𝜙 }]
>   | ∀ c, (𝑛, cd) ∈ 𝜙 ]
-}
noloopMonitors :: 𝛹 -> 𝐶 ↦ 𝒪s -> 𝐶 ↦ (OpDir ↦ Exp)
noloopMonitors 𝜓 = M.map (M.map ((...+) . map (noloopMonitor 𝜓)))

{- | Monitor sub-expression for a non-loop single synchronous channel operation.

For send operations:
After synchronization, its resource contribution is 1. After the rendezvous,
its resource contribution is 1 more.

For receive operations:
After synchronization, its resource contribution is 2.

Depends on: π, 𝑛, where 𝑛 ∈ dom(𝛱(π))

Reachability conditions for all processes:
  𝜓 = [π ↦ [𝑛 ↦ e | 𝑛 ∈ dom(𝛱(π))] | π ∈ dom(𝛱)]

Depnding on the operation direction, it produces:

>   ! ↦   if 𝜓(π)(𝑛) && 𝑛 < pc(π) then 1 else 0
>       + if 𝜓(π)(𝑛) && 𝑛 + 1 < pc(π) then 1 else 0
> 
>   ? ↦ if 𝜓(π)(𝑛) && 𝑛 < pc(π) then 2 else 0
-}
noloopMonitor :: 𝛹 -> 𝒪 -> Exp
noloopMonitor 𝜓 𝒪 {oP = p, oDir = d, o𝑛 = 𝑛} =
  let b = 𝜓 M.! p M.! 𝑛
      pc = π p
      synced = (𝑛 #) :< pc
      rendezvous = ((𝑛 + 1) #) :< pc
      monitor = case d of
        S -> IfElse (b :&& synced) (1 #) (0 #) :+ IfElse (b :&& rendezvous) (1 #) (0 #)
        R -> IfElse (b :&& synced) (2 #) (0 #)
   in monitor
