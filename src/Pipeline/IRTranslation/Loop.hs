module Pipeline.IRTranslation.Loop (loops) where

import Backend.Ast qualified as P'
import Backend.Utilities
import Data.Map qualified as M
import Data.Maybe (fromMaybe)
import IR.Ast
import IR.Utilities
import Pipeline.IRTranslation.Exps (parseExp)
import Pipeline.IRTranslation.Utilities

-- Collect all loops found in the program.
-- Assume that all loops are guarded by true.
loops :: Prog -> [Loop]
loops (Prog _ procs) =
  let zeros = 0 : zeros
      procs' = zip [0 ..] (zip zeros procs)
      process = processLoops (P'.ECon P'.CTrue)
   in concatMap (fst . uncurry process) procs'

-- Collect all loops found in a process.
processLoops :: P'.Exp -> Pid -> (PCounter, Stmt) -> ([Loop], PCounter)
processLoops e pid (n, s) = case s of
  -- Statement sequences merge the sets of loops produced by each sub-statement.
  Seq s1 s2 ->
    let (l1, n') = processLoops e pid (n, s1)
        (l2, n'') = processLoops e pid (n', s2)
     in (l1 ++ l2, n'')
  For x e1 e2 os ->
    let -- Decorate loop index with process id.
        x' = pid % x
        -- Gather all channel operations and the next available program
        -- point.
        (chops, n') = chanOps pid (n + 1) os
        l =
          [ Loop
              { -- Loop process
                pid = pid,
                -- Loop index
                var = x',
                -- Guard is at the entry program point
                guardP = n,
                -- Exit point is after the increment operation program point,
                -- which is the exit program point of the loop body
                exitP = n' + 1,
                -- Parse lower bound expression
                lower = parseExp e1,
                -- Parse upper bound expression
                upper = parseExp e2,
                -- Add channel operations
                chans = chops,
                -- Path expression collected during traversal
                pathexp = e
              }
          ]
     in (l, n' + 1)
  -- Atomic operations have no loops, but may offset the program counter.
  Atomic _ -> ([], n + ppOffset s)
  If e' s1 s2 ->
    let e'' = parseExp e'
        (l1, n') = processLoops (P'.And e e'') pid (n + 1, s1)
        (l2, n'') = processLoops (P'.And e (P'.Not e'')) pid (n', s2)
     in (l1 ++ l2, n'')
  -- Skip operations have no loops and do not offset the program counter.
  Skip -> ([], n)

-- Collect all channel operations in a loop.
-- Relevant information includes: channel name, program point
-- and direction.
chanOps :: Pid -> PCounter -> [Op] -> (ChMap ChOps, PCounter)
chanOps pid n =
  let addOp (chops, n') op =
        let -- Get channel name and direction
            (c, d) = (chName op, chDir op)
            -- Get program counters for channels
            pcs = fromMaybe M.empty (M.lookup c chops)
            -- Get list of channel operations for the direction
            dpcs = fromMaybe [] (M.lookup d pcs)
            ch =
              ChannelMeta
                { cmPid = pid,
                  cmVar = c,
                  cmOp = d,
                  cmPoint = n',
                  cmPathexp = (True ?)
                }
            pcs' = M.insert d (ch : dpcs) pcs
         in (M.insert c pcs' chops, n' + ppOffset op)
   in Prelude.foldl addOp (M.empty, n)