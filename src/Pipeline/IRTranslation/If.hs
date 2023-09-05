module Pipeline.IRTranslation.If (ifs) where

import IR.Ast
import Pipeline.IRTranslation.Exps (parseExp)
import Pipeline.IRTranslation.Meta.If
import Pipeline.IRTranslation.Utilities

-- | Collect all if statements found in the program.
ifs :: 𝑃 -> [ℐ]
ifs = programToCollection processIfs

-- | Collect all if statement found in a process.
processIfs :: 𝛬 -> 𝑆 -> [ℐ]
processIfs 𝛬 { p, 𝑛 } = \case
  If e s₁ s₂ ->
    let -- Process if branches continuation points.
        𝑛₁ = 𝑛 + 1 + ppOffset s₁
        𝑛₂ = 𝑛₁ + 1 + ppOffset s₂
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
     in [l]
  _ -> []