module Pipeline.IRTranslation.Invariant.WgBound (wgBounds) where

import Data.Set qualified as S

import Backend.Ast
import Backend.Utilities
import Pipeline.IRTranslation.Summary.WgOp

{- | Composes all channel bound invariants.
Depends on: 𝑊

Produces:

> ∀ w ∈ 𝑊. w ≥ 0
-}
wgBounds :: 𝑊 -> [Exp]
wgBounds = map wgBound . S.toList

{- | Constructs a wait group bound invariant.
Depends on: w ∈ 𝑊

Produces:

> w ≥ 0
-}
wgBound :: String -> Exp
wgBound w = (w @) :>= (0 #)
