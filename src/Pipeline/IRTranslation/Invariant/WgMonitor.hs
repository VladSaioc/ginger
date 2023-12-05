module Pipeline.IRTranslation.Invariant.WgMonitor (wgMonitors) where

import Data.List qualified as L
import Data.Map qualified as M
import Data.Maybe qualified as Mb

import Backend.Ast
import Backend.Utilities
import IR.Utilities
import Pipeline.IRTranslation.Meta.Loop
import Pipeline.IRTranslation.Meta.WgOp
import Pipeline.IRTranslation.Utilities
import Utilities.Collection

{- | Retrieves all WaitGroup monitor expressions by analyzing
all loop and non-loop WaitGroup operations. The produced expressions
represent a relationship between process progress and the number of
WaitGroup operations that should have already occurred.
Depends on:

1. Reachability conditions for all processes:
    𝜓 = [p ↦ [𝑛 ↦ e | 𝑛 ∈ dom(𝛯(p))] | p ∈ dom(𝛯)]
2. All program loops: [ℓ]
3. All non-loop operations:
    O = {(p, 𝑛, o) | (𝑛, o) ∉ op(ℓ), ℓ ∈ [ℓ], (𝑛, o) ∈ 𝜙, (p, 𝜙) ∈ 𝛯 }

Produces:

> [ w ↦ e | ∀ w. (𝑛, w.Add(e')) ∈ 𝜙, (p, 𝜙) ∈ 𝛯,
>     e =  𝛴 ∀ ℓ, (c, [Add ↦ e']) ∈ loopMonitor(ℓ). e'
>         + 𝛴 (p, 𝑛, !) ∈ O, e' = noloopMonitor(p, 𝑛). e' ]
-}
wgMonitors :: 𝛹 -> P ↦ (𝑋 ↦ 𝒲s) -> [ℒ] -> [Exp]
wgMonitors 𝜓 noloopOps ls =
  let noloopSubexps = L.map snd (M.toList (M.map (noloopMonitors 𝜓) noloopOps))
      loopSubexps = L.map (loopMonitor 𝜓) ls
      subexps = M.unionsWith (M.unionWith (:+)) (noloopSubexps ++ loopSubexps)
      wgMonitor w dir = (w @) :== Mb.fromMaybe (0 #) (M.lookup A dir)
   in M.elems $ M.mapWithKey wgMonitor subexps

{- | Monitor WaitGroup progress by analyzing the operations in a loop.
It returns an expression representing the resource contribution of each Waitgroup
operated on in a loop.
Depends on:

I. Reachability conditions for all processes:
    𝜓 = [p ↦ [𝑛 ↦ e | 𝑛 ∈ dom(𝛯(p))] | p ∈ dom(𝛯)]

II. ℓ = (p, O, x, e₁, e₂, 𝑛₀, 𝑛'), with the following properties:
1. p is the process id of the loop
2. O = {(𝑛₁, w₁.Add(e'₁)), ..., (𝑛ₘ, wₘ.Add(e'ₘ))} are loop WaitGroup operations.
3. x is the loop index variable
4. e₁ is the lower bound expression
5. e₂ is the upper bound expression
6. 𝑛 is the guard point
7. 𝑛' is the exit point

Produces:

> [ w ↦ [
>   Add ↦ if 𝜓(p)(𝑛₀) then
>           (x - e₁) * (𝛴 e'. ∀ (𝑛, w.Add(e')) ∈ O })
>         + (𝛴 ∀(𝑛, w.Add(e')) ∈ O.
>             if 𝑛 < 𝜋(p) < 𝑛' then e' else 0)
>       else 0 ]
>   | ∀ w, (𝑛, w.Add(e')) ∈ O ]
-}
loopMonitor :: 𝛹 -> ℒ -> 𝑋 ↦ (WgOpType ↦ Exp)
loopMonitor 𝜓 (ℒ {lP = p, l𝑋 = var, lower, l𝑛 = 𝑛, lExit = 𝑛', l𝒲s = wgs}) =
  let b = 𝜓 M.! p M.! 𝑛
      x = (var @)
      pc = 𝜋 p
      ext = (𝑛' #)
      singleOp 𝒲 {wDir = d, w𝑛 = 𝑛ᵢ, wE = e} =
        let synced = ((𝑛ᵢ #) :< pc) :&& (pc :< ext)
         in if d == A then IfElse synced e (0 #) else (0 #)
      concSize 𝒲 {wDir = d, wE = e} = if d == A then e else (0 #)
      subexp ops =
        let iterations = (x :- lower) :* (map concSize ops ...+)
            ops' = L.map singleOp ops
         in IfElse b (iterations :+ (ops' ...+)) (0 #)
   in M.map (M.map subexp) wgs

{- | Organize and compose under addition all non-loop monitor
sub-expressions for every WaitGroup for a given process.
Depends on: p, 𝜙

Reachability conditions for all processes:
  𝜓 = [p ↦ [𝑛 ↦ e | 𝑛 ∈ dom(𝛯(p))] | p ∈ dom(𝛯)]

Produces:

> [w ↦ [ Add ↦ { if 𝜓(p)(𝑛) && 𝑛 < 𝜋(p) then e' else 0 | ∀(𝑛, w.Add(e')) ∈ 𝜙 } ]
>   | ∀ w, (𝑛, w.Add(e')) ∈ 𝜙 ]
-}
noloopMonitors :: 𝛹 -> 𝑋 ↦ 𝒲s -> 𝑋 ↦ (WgOpType ↦ Exp)
noloopMonitors 𝜓 = M.map (M.map ((...+) . map (noloopMonitor 𝜓)))

{- | Monitor sub-expression for a non-loop single WaitGroup operation.

For add operations:
After the operation executes, its value is added to the WaitGroup counter.

Depends on: p, 𝑛, where 𝑛 ∈ dom(𝛯(p)), e

Reachability conditions for all processes:
  𝜓 = [p ↦ [𝑛 ↦ e | 𝑛 ∈ dom(𝛯(p))] | p ∈ dom(𝛯)]

Depnding on the operation direction, it produces:

>   Add ↦ if 𝜓(p)(𝑛) && 𝑛 < 𝜋(p) then e else 0
-}
noloopMonitor :: 𝛹 -> 𝒲 -> Exp
noloopMonitor 𝜓 𝒲 {wP = p, wDir = d, w𝑛 = 𝑛, wE = e} =
  let b = 𝜓 M.! p M.! 𝑛
      pc = 𝜋 p
      synced = (𝑛 #) :< pc
      monitor = case d of
        A -> IfElse (b :&& synced) e (0 #)
        _ -> (0 #)
   in monitor
