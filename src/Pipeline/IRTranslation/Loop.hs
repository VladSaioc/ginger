module Pipeline.IRTranslation.Loop (loops) where

import Backend.Ast qualified as P'
import Backend.Utilities
import Data.Map qualified as M
import Data.Maybe (fromMaybe)
import IR.Ast
import IR.Utilities
import Pipeline.IRTranslation.Exps (parseExp)
import Pipeline.IRTranslation.Meta.Channel
import Pipeline.IRTranslation.Meta.Loop
import Pipeline.IRTranslation.Utilities

-- Collect all loops found in the program.
-- Assume that all loops are initially guarded by true.
loops :: Prog -> [ℒ]
loops (Prog _ procs) =
  let zeros = 0 : zeros
      procs' = zip [0 ..] (zip zeros procs)
      process = processLoops (P'.ECon P'.CTrue)
   in concatMap (fst . uncurry process) procs'

-- Collect all loops found in a process.
processLoops :: P'.Exp -> P -> (P𝑛, Stmt) -> ([ℒ], P𝑛)
processLoops e p (𝑛, s) = case s of
  -- Statement sequences merge the sets of loops produced by each sub-statement.
  Seq s1 s2 ->
    let (l1, 𝑛') = processLoops e p (𝑛, s1)
        (l2, 𝑛'') = processLoops e p (𝑛', s2)
     in (l1 ++ l2, 𝑛'')
  For x e1 e2 os ->
    let -- Decorate loop index with process id.
        x' = p % x
        -- Gather all channel operations and the next available program
        -- point.
        (chops, 𝑛') = chanOps p (𝑛 + 1) os
        l =
          [ ℒ
              { -- Loop process
                lP = p,
                -- Loop index
                l𝑋 = x',
                -- Guard is at the entry program point
                lGuard = 𝑛,
                -- Exit point is after the increment operation program point,
                -- which is the exit program point of the loop body
                lExit = 𝑛' + 1,
                -- Parse lower bound expression
                lower = parseExp e1,
                -- Parse upper bound expression
                upper = parseExp e2,
                -- Add channel operations
                l𝒪s = chops,
                -- Path expression collected during traversal
                lPathexp = e
              }
          ]
     in (l, 𝑛' + 1)
  -- Atomic operations have no loops, but may offset the program counter.
  Atomic _ -> ([], 𝑛 + ppOffset s)
  If e' s1 s2 ->
    let e'' = parseExp e'
        (l1, 𝑛') = processLoops (e P'.:&& e'') p (𝑛 + 1, s1)
        (l2, 𝑛'') = processLoops (e P'.:&& P'.Not e'') p (𝑛' + 1, s2)
     in (l1 ++ l2, 𝑛'')
  -- Skip operations have no loops and do not offset the program counter.
  Skip -> ([], 𝑛)

-- Collect all channel operations in a loop.
-- Relevant information includes: channel name, program point
-- and direction.
chanOps :: P -> P𝑛 -> [Op] -> (𝐶 ↦ 𝒪s, P𝑛)
chanOps p 𝑛 =
  let addOp (chops, 𝑛') op =
        let -- Get channel name and direction
            (c, d) = (chName op, chDir op)
            -- Get program counters for channel operations
            𝑛s = fromMaybe M.empty (M.lookup c chops)
            -- Get list of channel operations for the direction
            dpcs = fromMaybe [] (M.lookup d 𝑛s)
            ch =
              𝒪
                { oP = p,
                  o𝐶 = c,
                  oDir = d,
                  o𝑛 = 𝑛',
                  oPathexp = (True ?)
                }
            pcs' = M.insert d (ch : dpcs) 𝑛s
         in (M.insert c pcs' chops, 𝑛' + ppOffset op)
   in Prelude.foldl addOp (M.empty, 𝑛)