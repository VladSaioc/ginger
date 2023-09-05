module Pipeline.IRTranslation.Return where

import IR.Ast
import Pipeline.IRTranslation.Meta.Return
import Pipeline.IRTranslation.Utilities

-- | Collect all return statements in the program.
-- Assume that all return statements are initially guarded by true.
returns :: 𝑃 -> [ℛ]
returns = programToCollection processReturns

-- | Collect all return statements found in a process.
processReturns :: 𝛬 -> 𝑆 -> [ℛ]
processReturns 𝜆 = \case
  -- Return statements only increment the program counter.
  Return -> [ℛ {r𝑛 = 𝑛 𝜆, rP = p 𝜆}]
  _ -> []
