module Pipeline.IRTranslation.Return where

import IR.Ast
import Pipeline.IRTranslation.Meta.Return
import Pipeline.IRTranslation.Utilities

-- | Collect all return statements in the program.
-- Assume that all return statements are initially guarded by true.
returns :: 𝑃 -> [ℛ]
returns (𝑃 _ procs) =
  let zeros = 0 : zeros
      procs' = zip [0 ..] (zip zeros procs)
   in concatMap (fst . uncurry processReturns) procs'

-- | Collect all return statements found in a process.
processReturns :: P -> (𝑁, 𝑆) -> ([ℛ], 𝑁)
processReturns p (𝑛, s) =
  let 𝑛' = 𝑛 + ppOffset s
      get = processReturns p
   in case s of
        -- Skip operations have no loops and do not offset the program counter.
        Skip -> ([], 𝑛')
        -- Return statements only increment the program counter.
        Return -> ([ℛ {r𝑛 = 𝑛, rP = p}], 𝑛')
        -- Statement sequences merge the sets of loops produced by each sub-statement.
        Seq s1 s2 ->
          let (r₁, 𝑛₁) = get (𝑛, s1)
              (r₂, 𝑛₂) = get (𝑛₁, s2)
           in (r₁ ++ r₂, 𝑛₂)
        For {} -> ([], 𝑛')
        -- Atomic operations have no loops, but may offset the program counter.
        Atomic _ -> ([], 𝑛')
        If _ s1 s2 ->
          let (r₁, 𝑛₁) = get (𝑛 + 1, s1)
              (r₂, 𝑛₂) = get (𝑛₁ + 1, s2)
           in (r₁ ++ r₂, 𝑛₂)