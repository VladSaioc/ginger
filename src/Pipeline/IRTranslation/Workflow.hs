module Pipeline.IRTranslation.Workflow (irToBackend) where

import Backend.Ast (Program (Program))
import Backend.Simplifier qualified as T (simplify)
import IR.Ast
import IR.Homogeneity (homogeneous)
import IR.SanityCheck (sanityCheck)
import IR.Simplifier (simplify)
import IR.Stratification (stratified)
import Pipeline.IRTranslation.Boilerplate (wholeEncoding)
import Pipeline.IRTranslation.ChInstructions (noloopPsChanInsns)
import Pipeline.IRTranslation.Channels (caps)
import Pipeline.IRTranslation.FreeVars (fvs)
import Pipeline.IRTranslation.If (ifs)
import Pipeline.IRTranslation.Loop (loops)
import Pipeline.IRTranslation.Processes (procs)
import Pipeline.IRTranslation.Reachability (reachability)
import Pipeline.IRTranslation.Return (returns)
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
  (𝜎, ts) <- fvs p
  let 𝜓 = reachability p
  let ls = loops p
  let is = ifs p
  let k = caps p
  let ps = procs k p
  let as = noloopPsChanInsns p
  let rs = returns p
  let prog = wholeEncoding 𝜓 𝜎 ts k ps as is ls rs
  return $ T.simplify prog
