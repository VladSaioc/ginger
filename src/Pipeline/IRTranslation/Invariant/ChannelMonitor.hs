module Pipeline.IRTranslation.Invariant.ChannelMonitor (channelMonitors) where

import Backend.Ast
import Backend.Utilities
import Data.Map qualified as M
import Data.Maybe qualified as Mb
import Pipeline.IRTranslation.Invariant.ChannelAsyncMonitor (asyncChannelMonitors)
import Pipeline.IRTranslation.Invariant.ChannelSyncMonitor (syncChannelMonitors)
import Pipeline.IRTranslation.Meta.Channel
import Pipeline.IRTranslation.Meta.Loop
import Pipeline.IRTranslation.Utilities
import Utilities.Collection

{- | Produces channel monitors by composing both buffered and
unbuffered channel behaviour under an expression conditional
over the capacity expression.

Depends on:

> 1. Reachability conditions for all processes:
>     𝜓 = [p ↦ [𝑛 ↦ e | 𝑛 ∈ dom(𝛯(p))] | p ∈ dom(𝛯)]
> 2. All program loops: [ℓ]
> 3. All non-loop operations:
>     O = {(p, 𝑛, o) | (𝑛, o) ∉ op(ℓ), ℓ ∈ [ℓ], (𝑛, o) ∈ 𝜙, (p, 𝜙) ∈ 𝛯 }
> 4. Channel capacity environments: 𝜅
> 
> ∀ c, e1 = syncChannelMonitor(𝜓, O, [ℓ])(c),
>      e2 = asyncChannelMonitor(𝜓, O, [ℓ])(c).
>     c = if 𝜅(c) > 0 then e2 else e1
-}
channelMonitors :: 𝛹 -> 𝛫 -> P ↦ (𝐶 ↦ 𝒪s) -> [ℒ] -> [Exp]
channelMonitors 𝜓 𝜅 noloopOps ls =
  let syncMs = syncChannelMonitors 𝜓 noloopOps ls
      asyncMs = asyncChannelMonitors 𝜓 noloopOps ls
      combineMonitors c s a =
        let cap = Mb.fromMaybe (0 #) (M.lookup c 𝜅)
            isAsync = cap :> (0 #)
         in (c @) :== IfElse isAsync a s
   in M.elems $ M.unionWithKey combineMonitors syncMs asyncMs
