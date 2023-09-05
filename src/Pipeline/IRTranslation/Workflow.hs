module Pipeline.IRTranslation.Workflow (irToBackend) where

import Backend.Ast (Program)
import Backend.Simplifier qualified as T (simplify)
import IR.Ast
import IR.Homogeneity (homogeneous)
import IR.SanityCheck (sanityCheck)
import IR.Simplifier (simplify)
import IR.Stratification (stratified)
import Pipeline.IRTranslation.Boilerplate (wholeEncoding)
import Pipeline.IRTranslation.Context.Capacity (caps)
import Pipeline.IRTranslation.Context.Reachability (reachability)
import Pipeline.IRTranslation.Context.TypeInference (typesAndFvs)
import Pipeline.IRTranslation.Processes (procs)
import Pipeline.IRTranslation.Meta.Meta (meta)
import Utilities.Err

-- | Convert IR program to back-end program. May fail.
irToBackend :: 𝑃 -> Err Program
irToBackend p' = do
  let p = simplify p'
  _ <- sanityCheck p
  -- _ <- homogeneous p
  -- _ <-
  --   multiGuard
  --     [ (not (stratified p), "Program is not stratified")
  --     ]
  (𝜎, ts) <- typesAndFvs p
  let 𝜅 = caps p
  let 𝜓 = reachability p
  let 𝜉 = procs 𝜅 p
  let 𝓂 = meta 𝜅 p
  let prog = wholeEncoding 𝜓 𝜎 ts 𝜅 𝜉 𝓂
  return $ T.simplify prog
