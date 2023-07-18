module Pipeline.IRTranslation.ChInstructions (chanOps, noloopPsChanInsns) where

import Data.List qualified as L
import Data.Map qualified as M
import Data.Maybe qualified as Mb
import IR.Ast
import IR.Utilities
import Pipeline.IRTranslation.Utilities

{- Aggregate all channel operation points from a given map of program points.
Produces a set of triples consisting of channel name, direction and
program point.
Depends on: ϕ

Produces:
{ (c, d, n) | (n, cd) ∈ ϕ \ loop(ϕ). d ∈ {!, ?} }
-}
chanOps :: ProgPoints -> [(Ch, OpDir, PCounter)]
chanOps =
  let insn n s = do
        op <- backendChannelOp s
        let (c, d) = either (,S) (,R) op
        return (c, d, n)
   in Mb.catMaybes . M.elems . M.mapWithKey insn

-- Aggregates all non-loop channel operations across
-- all processes of the program, including operation
-- direction, program point, and channel name.
noloopPsChanInsns :: Prog -> PChInsns
noloopPsChanInsns (Prog _ ps) = M.fromList (zip [0 ..] (L.map (fst . noloopPChanInsns 0) ps))

-- Aggregates all non-loop channel operations, including operation
-- direction, program point, and channel name.
-- Depends on: n, S
--
-- Produces:
-- [SEND]: ⟨n, c!⟩ -> ⟨n + 1, [c ↦ [! ↦ {n}]]⟩
-- [RECV]: ⟨n, c?⟩ -> ⟨n + 1, [c ↦ [? ↦ {n}]]⟩
-- [FOR]: ⟨n, for (i : e .. e) { s }⟩ -> ⟨n + |s| + 2, []⟩
-- [SEQ]: ⟨n, S₁; S₂⟩ -> ⟨n'', M⟩
--         |- ⟨n, S₁⟩ -> ⟨n', M₁⟩
--         |- ⟨n', S₂⟩ -> ⟨n'', M₂⟩
--         |- M = [c ↦ os | c ∈ dom(M₁) ∪ dom(M₂),
--                          os = [d ↦ { n | n ∈ M₁(c)(d) ∪ M₂(c)(d)} | d ∈ dom(M₁(c)) ∪ dom(M₂(c)) ]]
noloopPChanInsns :: PCounter -> Stmt -> (ChMap ChOps, PCounter)
noloopPChanInsns n = \case
  -- Sequence maps are aggregated via point-wise union
  Seq s1 s2 ->
    let (o1, n') = noloopPChanInsns n s1
        (o2, n'') = noloopPChanInsns n' s2
     in (o1 ⊎ o2, n'')
  Skip -> (M.empty, n)
  -- Loops are handled separately
  For _ _ _ os -> (M.empty, n + L.length os + 2)
  -- Atomic operations are added to the list of triples.
  Atomic o ->
    let (c, d) = (chName o, chDir o)
     in ((c, d, n) +> M.empty, n + 1)