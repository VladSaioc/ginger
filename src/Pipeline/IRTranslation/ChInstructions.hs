module Pipeline.IRTranslation.ChInstructions (noloopPsChanInsns) where

import Backend.Ast qualified as P'
import Backend.Utilities
import Data.Map qualified as M
import IR.Ast
import IR.Utilities
import Pipeline.IRTranslation.Exps (parseExp)
import Pipeline.IRTranslation.Meta.Channel
import Pipeline.IRTranslation.Utilities

-- Aggregates all non-loop channel operations across
-- all processes of the program, including operation
-- direction, program point, and channel name.
noloopPsChanInsns :: 𝑃 -> P ↦ (𝐶 ↦ 𝒪s)
noloopPsChanInsns (𝑃 _ ps) =
  let ps' = zip [0 ..] ps
   in M.mapWithKey (\i p -> fst $ noloopPChanInsns i (True ?) 0 p) $ M.fromList ps'

-- Aggregates all non-loop channel operations, including operation
-- direction, program point, and channel name.
-- Depends on: n, S
--
-- Produces:
-- [SEND]: ⟨n, c!⟩ -> ⟨n + 2, [c ↦ [! ↦ {n}]]⟩
-- [RECV]: ⟨n, c?⟩ -> ⟨n + 1, [c ↦ [? ↦ {n}]]⟩
-- [FOR]: ⟨n, for (i : e .. e) { s }⟩ -> ⟨n + |s| + 2, []⟩
-- [SEQ]: ⟨n, S₁; S₂⟩ -> ⟨n'', M⟩
--         |- ⟨n, S₁⟩ -> ⟨n', M₁⟩
--         |- ⟨n', S₂⟩ -> ⟨n'', M₂⟩
--         |- M = [c ↦ os | c ∈ dom(M₁) ∪ dom(M₂),
--                          os = [d ↦ { n | n ∈ M₁(c)(d) ∪ M₂(c)(d)} | d ∈ dom(M₁(c)) ∪ dom(M₂(c)) ]]
noloopPChanInsns :: P -> P'.Exp -> P𝑛 -> 𝑆 -> (𝐶 ↦ 𝒪s, P𝑛)
noloopPChanInsns p b 𝑛 s =
  let 𝑛' = 𝑛 + ppOffset s
   in case s of
        -- Sequence maps are aggregated via point-wise union
        Seq s1 s2 ->
          let (o1, 𝑛1) = noloopPChanInsns p b 𝑛 s1
              (o2, 𝑛2) = noloopPChanInsns p b 𝑛1 s2
           in (o1 ⊎ o2, 𝑛2)
        Skip -> (M.empty, 𝑛')
        -- Loops are handled separately
        For {} -> (M.empty, 𝑛')
        -- Atomic operations are added to the list of triples.
        Atomic o ->
          let (c, d) = (chName o, chDir o) in (𝒪 {oP = p, o𝐶 = c, oDir = d, o𝑛 = 𝑛, oPathexp = b} +> M.empty, 𝑛')
        If b' s1 s2 ->
          let b'' = parseExp b'
              (o1, n1) = noloopPChanInsns p (b'' P'.:&& b) (𝑛 + 1) s1
              (o2, n2) = noloopPChanInsns p (P'.Not b'' P'.:&& b) n1 s2
           in (o1 ⊎ o2, n2)
