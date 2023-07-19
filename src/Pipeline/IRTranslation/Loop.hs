module Pipeline.IRTranslation.Loop (loops) where

import Data.Map qualified as M
import Data.Maybe (fromMaybe)
import Data.Set qualified as S
import IR.Ast
import IR.Utilities
import Pipeline.IRTranslation.Exps (parseExp)
import Pipeline.IRTranslation.Utilities

loops :: Prog -> [Loop]
loops (Prog _ procs) =
  let zeros = 0 : zeros
      procs' = zip [0 ..] (zip zeros procs)
   in concatMap (fst . uncurry processLoops) procs'

-- Collect all loops found in a process.
processLoops :: Pid -> (PCounter, Stmt) -> ([Loop], PCounter)
processLoops pid (n, s) = case s of
  Seq s1 s2 ->
    let (l1, n') = processLoops pid (n, s1)
        (l2, n'') = processLoops pid (n', s2)
     in (l1 ++ l2, n'')
  For x e1 e2 os ->
    let x' = pid % x
        (chops, n') = chanOps (n + 1) os
        l =
          [ Loop
              { pid = pid,
                var = x',
                guardP = n,
                exitP = n' + 1,
                lower = parseExp e1,
                upper = parseExp e2,
                chans = chops
              }
          ]
     in (l, n' + 1)
  Atomic _ -> ([], n + ppOffset s)
  Skip -> ([], n)

-- Collect all channel operations in a loop.
-- Relevant information includes: channel name, program point
-- and direction.
chanOps :: PCounter -> [Op] -> (ChMap ChOps, PCounter)
chanOps n =
  let addOp (chops, n') op =
        let -- Get channel name and direction
            (c, d) = (chName op, chDir op)
            -- Get program counters for channels
            pcs = fromMaybe M.empty (M.lookup c chops)
            -- Get sub-set of counter for
            dpcs = fromMaybe S.empty (M.lookup d pcs)
            pcs' = M.insert d (S.insert n' dpcs) pcs
         in (M.insert c pcs' chops, n' + ppOffset op)
   in Prelude.foldl addOp (M.empty, n)