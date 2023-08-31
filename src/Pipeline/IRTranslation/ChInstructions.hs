module Pipeline.IRTranslation.ChInstructions (noloopPsChanInsns) where

import Data.Map qualified as M
import IR.Ast
import IR.Utilities
import Pipeline.IRTranslation.Meta.Channel
import Pipeline.IRTranslation.Utilities
import Utilities.Collection

-- | Aggregates all non-loop channel operations across
-- all processes of the program, including operation
-- direction, program point, and channel name.
noloopPsChanInsns :: 𝑃 -> P ↦ (𝐶 ↦ 𝒪s)
noloopPsChanInsns (𝑃 _ ps) =
  let ps' = zip [0 ..] ps
   in M.mapWithKey (\i -> fst . noloopPChanInsns i 0) $ M.fromList ps'

{- | Aggregates all non-loop channel operations, including operation
direction, program point, and channel name.
Depends on: 𝑛, S

Rules:

> [SKIP]:    ⟨𝑛, skip⟩ -> ⟨𝑛, []⟩
> [RETURN]:  ⟨𝑛, return⟩ -> ⟨𝑛 + 1, []⟩
> [SEND]:    ⟨𝑛, c!⟩ -> ⟨𝑛 + 2, [c ↦ [! ↦ {𝑛}]]⟩
> [RECV]:    ⟨𝑛, c?⟩ -> ⟨𝑛 + 1, [c ↦ [? ↦ {𝑛}]]⟩
> [FOR]:     ⟨𝑛, for (i : e .. e) { s }⟩ -> ⟨𝑛 + |s| + 2, []⟩
> [SEQ]:     ⟨𝑛, S₁; S₂⟩ -> ⟨𝑛'', M₁ ⊔ M₂⟩
>            |- ⟨𝑛, S₁⟩ -> ⟨𝑛', M₁⟩
>            |- ⟨𝑛', S₂⟩ -> ⟨𝑛'', M₂⟩
-}
noloopPChanInsns :: P -> 𝑁 -> 𝑆 -> (𝐶 ↦ 𝒪s, 𝑁)
noloopPChanInsns p 𝑛 s =
  let 𝑛' = 𝑛 + ppOffset s
      get = noloopPChanInsns p
   in case s of
        -- Sequence maps are aggregated via point-wise union
        Seq s₁ s₂ ->
          let (o₁, 𝑛₁) = get 𝑛 s₁
              (o₂, 𝑛₂) = get 𝑛₁ s₂
           in (o₁ ∪ o₂, 𝑛₂)
        Skip -> (M.empty, 𝑛')
        Return -> (M.empty, 𝑛')
        -- Loops are handled separately
        For {} -> (M.empty, 𝑛')
        -- Atomic operations are added to the list of triples.
        Atomic o ->
          let (c, d) = (chName o, chDir o)
              o' = 𝒪 {oP = p, o𝐶 = c, oDir = d, o𝑛 = 𝑛}
           in (o' +> M.empty, 𝑛')
        If _ s₁ s₂ ->
          let (o₁, 𝑛₁) = get (𝑛 + 1) s₁
              (o₂, 𝑛₂) = get (𝑛₁ + 1) s₂
           in (o₁ ∪ o₂, 𝑛₂)
