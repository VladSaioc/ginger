module Pipeline.IRTranslation.If (ifs) where

import IR.Ast
import Pipeline.IRTranslation.Exps (parseExp)
import Pipeline.IRTranslation.Meta.If
import Pipeline.IRTranslation.Utilities

-- Collect all if statements found in the program.
ifs :: Prog -> [ℐ]
ifs (Prog _ procs) =
  let zeros = 0 : zeros
      procs' = zip [0 ..] (zip zeros procs)
   in concatMap (fst . uncurry processIfs) procs'

-- Collect all if statement found in a process.
processIfs :: P -> (P𝑛, Stmt) -> ([ℐ], P𝑛)
processIfs p (𝑛, s) = case s of
  -- Statement sequences merge the sets of loops produced by each sub-statement.
  Seq s1 s2 ->
    let (l1, 𝑛') = processIfs p (𝑛, s1)
        (l2, 𝑛'') = processIfs p (𝑛', s2)
     in (l1 ++ l2, 𝑛'')
  For {} -> ([], 𝑛 + ppOffset s)
  -- Atomic operations have no ifs, but may offset the program counter.
  Atomic _ -> ([], 𝑛 + ppOffset s)
  If e s1 s2 ->
    let -- Process if branches continuation points.
        (l1, 𝑛') = processIfs p (𝑛 + 1, s1)
        (l2, 𝑛'') = processIfs p (𝑛' + 1, s2)
        l =
          ℐ
            { -- Loop process
              iP = p,
              -- Parse lower bound expression
              iGuardExp = parseExp e,
              -- Guard is at the entry program point
              iGuard = 𝑛,
              -- Exit point is after the increment operation program point,
              -- which is the exit program point of the loop body
              iElse = 𝑛' + 1,
              -- Parse upper bound expression
              iExit = 𝑛''
            }
     in (l : l1 ++ l2, 𝑛' + 1)
  Skip -> ([], 𝑛)