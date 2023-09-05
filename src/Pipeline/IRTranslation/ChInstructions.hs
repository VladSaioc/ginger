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
noloopPsChanInsns = programToCollection noloopPChanInsns

{- | Aggregates all non-loop channel operations, including operation
direction, program point, and channel name.
Depends on: 𝑛, p, p', S

Rules:

> [SKIP]:    𝑛, p, p' ⊢ skip -> []

> [RETURN]:  𝑛, p, p' ⊢ return -> []

> [FOR]:     𝑛, p, p' ⊢ for (i : e .. e) { s } -> []

> [SEND]:    𝑛, p, p' ⊢ c! -> p ↦ [c ↦ [! ↦ {𝑛}]]

> [RECV]:    𝑛, p, p' ⊢ c? -> p ↦ [c ↦ [? ↦ {𝑛}]]

> [SEQ]:     ⟨𝑛, 𝑆₁; 𝑆₂⟩ -> M₁ ⊔ M₂
>            ↳ ⟨𝑛, 𝑆₁⟩ -> M₁
>            ↳ ⟨𝑛', 𝑆₂⟩ -> M₂

> [IF]:
-}
noloopPChanInsns :: 𝛬 -> 𝑆 -> P ↦ (𝐶 ↦ 𝒪s)
noloopPChanInsns 𝛬 { 𝑛, p } = \case
   -- Atomic operations are added to the list of triples.
   Atomic o ->
     let (c, d) = (chName o, chDir o)
         o' = 𝒪 {oP = p, o𝐶 = c, oDir = d, o𝑛 = 𝑛}
      in M.empty ⇒ (p, o' +> M.empty)
   -- All other statements return an empty map, or are traversed
   -- recursively in inductive cases.
   _ -> M.empty
