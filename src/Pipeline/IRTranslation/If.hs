module Pipeline.IRTranslation.If (ifs) where

import IR.Ast
import Pipeline.IRTranslation.Exps (parseExp)
import Pipeline.IRTranslation.Meta.If
import Pipeline.IRTranslation.Utilities

-- | Collect all if statements found in the program.
ifs :: 𝑃 -> [ℐ]
ifs (𝑃 _ procs) =
  let zeros = 0 : zeros
      procs' = zip [0 ..] (zip zeros procs)
   in concatMap (fst . uncurry processIfs) procs'

-- | Collect all if statement found in a process.
processIfs :: P -> (𝑁, 𝑆) -> ([ℐ], 𝑁)
processIfs p (𝑛, s) = case s of
  Skip -> ([], 𝑛)
  Return -> ([], 𝑛 + 1)
  -- Statement sequences merge the sets of loops produced by each sub-statement.
  Seq s1 s2 ->
    let (l1, 𝑛₁) = processIfs p (𝑛, s1)
        (l2, 𝑛₂) = processIfs p (𝑛₁, s2)
     in (l1 ++ l2, 𝑛₂)
  For {} -> ([], 𝑛 + ppOffset s)
  -- Atomic operations have no ifs, but may offset the program counter.
  Atomic _ -> ([], 𝑛 + ppOffset s)
  If e s1 s2 ->
    let -- Process if branches continuation points.
        (l1, 𝑛₁) = processIfs p (𝑛 + 1, s1)
        (l2, 𝑛₂) = processIfs p (𝑛₁ + 1, s2)
        l =
          ℐ
            { -- Loop process
              iP = p,
              -- Parse lower bound expression
              iGuard = parseExp e,
              -- Guard is at the conditional entry program point
              i𝑛 = 𝑛,
              -- Else branch program point
              iElse = 𝑛₁ + 1,
              -- Conditional exit program point
              iExit = 𝑛₂
            }
     in (l : l1 ++ l2, 𝑛₂)