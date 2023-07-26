module Pipeline.IRTranslation.ChInstructions (noloopPsChanInsns) where

import Backend.Ast qualified as P'
import Backend.Utilities
import Data.Map qualified as M
import IR.Ast
import IR.Utilities
import Pipeline.IRTranslation.Exps (parseExp)
import Pipeline.IRTranslation.Utilities

-- Aggregates all non-loop channel operations across
-- all processes of the program, including operation
-- direction, program point, and channel name.
noloopPsChanInsns :: Prog -> PChInsns
noloopPsChanInsns (Prog _ ps) =
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
noloopPChanInsns :: Pid -> P'.Exp -> PCounter -> Stmt -> (ChMap ChOps, PCounter)
noloopPChanInsns pid e n s =
  let n' = n + ppOffset s
   in case s of
        -- Sequence maps are aggregated via point-wise union
        Seq s1 s2 ->
          let (o1, n1) = noloopPChanInsns pid e n s1
              (o2, n2) = noloopPChanInsns pid e n1 s2
           in (o1 ⊎ o2, n2)
        Skip -> (M.empty, n')
        -- Loops are handled separately
        For {} -> (M.empty, n')
        -- Atomic operations are added to the list of triples.
        Atomic o ->
          let (c, d) = (chName o, chDir o)
              ch =
                ChannelMeta
                  { cmPid = pid,
                    cmVar = c,
                    cmOp = d,
                    cmPoint = n,
                    cmPathexp = e
                  }
           in (ch +> M.empty, n')
        If e' s1 s2 ->
          let e'' = parseExp e'
              (o1, n1) = noloopPChanInsns pid (P'.And e'' e) (n + 1) s1
              (o2, n2) = noloopPChanInsns pid (P'.And (P'.Not e'') e) n1 s2
           in (o1 ⊎ o2, n2)
