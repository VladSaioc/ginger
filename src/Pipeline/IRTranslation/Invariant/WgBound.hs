module Pipeline.IRTranslation.Invariant.WgBound (wgBounds) where

import Backend.Ast
import Backend.Utilities
import Data.Set qualified as S
import Pipeline.IRTranslation.Meta.WgOp

{- | Composes all channel bound invariants.
Depends on: 𝜅

Produces:

> ∀ c ∈ dom(𝜅). channelBound(c, 𝜅(c))
-}
wgBounds :: 𝑊 -> [Exp]
wgBounds = map wgBound . S.toList

{- | Constructs a channel bound invariant.
Depends on: w ∈ 𝑊

Produces:

> w ≥ 0
-}
wgBound :: String -> Exp
wgBound w = (w @) :>= (0 #)
