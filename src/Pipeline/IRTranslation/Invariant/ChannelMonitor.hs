module Pipeline.IRTranslation.Invariant.ChannelMonitor (channelMonitors) where

import Backend.Ast
import Backend.Utilities
import Data.Map qualified as M
import Data.Maybe qualified as Mb
import Pipeline.IRTranslation.Invariant.ChannelAsyncMonitor (asyncChannelMonitors)
import Pipeline.IRTranslation.Invariant.ChannelSyncMonitor (syncChannelMonitors)
import Pipeline.IRTranslation.Utilities

{- Produces channel monitors by composing both buffered and
unbuffered channel behaviour under conjoined implications.

Depends on:
1. All program loops: [ℓ]
2. All non-loop operations:
    O = {(π, n, o) | (n, o) ∉ op(ℓ), ℓ ∈ [ℓ], (n, o) ∈ ϕ, (π, ϕ) ∈ Π }
3. Channel capacity environments: κ

∀ c, e1 = syncChannelMonitor(O, [ℓ])(c),
     e2 = asyncChannelMonitor(O, [ℓ])(c).
    (κ(c) = 0 ⇒ c = e1)
  ∧ (κ(c) > 0 ⇒ c = e2)
-}
channelMonitors :: KEnv -> PChInsns -> [Loop] -> [Exp]
channelMonitors kenv noloopOps ls =
  let syncMs = syncChannelMonitors noloopOps ls
      asyncMs = asyncChannelMonitors noloopOps ls
      combineMonitors c s a =
        let cap = Mb.fromMaybe (0 #) (M.lookup c kenv)
            isSync = Eq (0 #) cap
            isAsync = Lt (0 #) cap
            eqc = Eq (c @)
         in And (Implies isSync $ eqc s) (Implies isAsync $ eqc a)
   in M.elems $ M.unionWithKey combineMonitors syncMs asyncMs
