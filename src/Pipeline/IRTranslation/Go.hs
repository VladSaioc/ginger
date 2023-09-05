module Pipeline.IRTranslation.Go (gos) where

import IR.Ast
import Pipeline.IRTranslation.Meta.Go
import Pipeline.IRTranslation.Utilities

-- | Collect all if statements found in the program.
gos :: 𝑃 -> [𝒢]
gos = programToCollection processGos

-- | Collect all if statement found in a process.
processGos :: 𝛬 -> 𝑆 -> [𝒢]
processGos 𝛬 { p, 𝑛, nextp } = \case
  Go {} ->
    let g =
          𝒢
            { -- Go process
              gP = p,
              -- Child goroutine
              gP' = nextp,
              -- Program point
              g𝑛 = 𝑛
            }
     in [g]
  _ -> []