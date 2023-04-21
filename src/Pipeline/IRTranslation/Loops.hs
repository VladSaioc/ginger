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
   in concatMap (fst . uncurry procBounds) procs'

procBounds :: Pid -> (PCounter, Stmt) -> ([Loop], PCounter)
procBounds pid (n, s) = case s of
  Seq s1 s2 ->
    let (ls1, n') = procBounds pid (n, s1)
        (ls2, n'') = procBounds pid (n', s2)
     in (ls1 ++ ls2, n'')
  For x e1 e2 os ->
    let x' = pid % x
        (chops, n') = chanOps (n + 1) os
        loop =
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
     in (loop, n' + 1)
  Atomic _ -> ([], n + 1)
  Skip -> ([], n)

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
         in (M.insert c pcs' chops, n' + 1)
   in Prelude.foldl addOp (M.empty, n)