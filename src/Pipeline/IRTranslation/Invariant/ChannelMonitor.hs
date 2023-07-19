module Pipeline.IRTranslation.Invariant.ChannelMonitor (channelMonitors) where

import Backend.Ast
import Backend.Utilities
import Data.Map qualified as M
import Data.Maybe qualified as Mb
import Pipeline.IRTranslation.Invariant.ChannelAsyncMonitor (asyncChannelMonitors)
import Pipeline.IRTranslation.Invariant.ChannelSyncMonitor (syncChannelMonitors)
import Pipeline.IRTranslation.Utilities

{- Produces channel monitors by composing both buffered and
unbuffered channel behaviour under an expression conditional
over the capacity expression.

Depends on:
1. All program loops: [ℓ]
2. All non-loop operations:
    O = {(π, n, o) | (n, o) ∉ op(ℓ), ℓ ∈ [ℓ], (n, o) ∈ ϕ, (π, ϕ) ∈ Π }
3. Channel capacity environments: κ

∀ c, e1 = syncChannelMonitor(O, [ℓ])(c),
     e2 = asyncChannelMonitor(O, [ℓ])(c).
    c = if κ(c) > 0 then e2 else e1
-}
channelMonitors :: KEnv -> PChInsns -> [Loop] -> [Exp]
channelMonitors kenv noloopOps ls =
  let syncMs = syncChannelMonitors noloopOps ls
      asyncMs = asyncChannelMonitors noloopOps ls
      combineMonitors c s a =
        let cap = Mb.fromMaybe (0 #) (M.lookup c kenv)
            isAsync = Lt (0 #) cap
         in Eq (c @) (IfElse isAsync a s)
   in M.elems $ M.unionWithKey combineMonitors syncMs asyncMs
