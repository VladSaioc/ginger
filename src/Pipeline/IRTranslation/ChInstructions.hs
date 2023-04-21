module Pipeline.IRTranslation.ChInstructions (chanOps, noloopPsChanInsns) where

import Data.List qualified as L
import Data.Map qualified as M
import Data.Maybe qualified as Mb
import IR.Ast
import IR.Utilities
import Pipeline.IRTranslation.Utilities

{- Aggregate all channel operation points from a given map of points.
Produces a set of triples consisting of channel name, direction and
program point.
Depends on: ϕ
-}
chanOps :: ProgPoints -> [(Ch, OpDir, PCounter)]
chanOps =
  let insn (n, s) =
        case (s !?) of
          (True, c) -> Just (c, S, n)
          _ -> case (s ??) of
            (True, c) -> Just (c, R, n)
            _ -> Nothing
   in map (Mb.fromJust . insn) . M.toList

noloopPsChanInsns :: Prog -> PChInsns
noloopPsChanInsns (Prog _ ps) = M.fromList (zip [0 ..] (L.map (fst . noloopPChanInsns 0) ps))

noloopPChanInsns :: PCounter -> Stmt -> (ChMap ChOps, PCounter)
noloopPChanInsns n = \case
  Seq s1 s2 ->
    let (o1, n') = noloopPChanInsns n s1
        (o2, n'') = noloopPChanInsns n' s2
     in (o1 |+| o2, n'')
  Skip -> (M.empty, n)
  For _ _ _ os -> (M.empty, n + L.length os + 2)
  Atomic o ->
    let (c, d) = (chName o, chDir o)
     in (M.empty +> (c, d, n), n + 1)