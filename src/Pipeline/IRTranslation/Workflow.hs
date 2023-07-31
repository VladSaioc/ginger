module Pipeline.IRTranslation.Workflow (irToBackend) where

import Backend.Ast (Program (Program))
import IR.Ast
import IR.Homogeneity (homogeneous)
import IR.SanityCheck (sanityCheck)
import IR.Simplifier (simplify)
import IR.Stratification (stratified)
import Pipeline.IRTranslation.Boilerplate (wholeEncoding)
import Pipeline.IRTranslation.ChInstructions (noloopPsChanInsns)
import Pipeline.IRTranslation.Channels (getCaps)
import Pipeline.IRTranslation.FreeVars (fvs)
import Pipeline.IRTranslation.If (ifs)
import Pipeline.IRTranslation.Loop (loops)
import Pipeline.IRTranslation.Processes (getProcs)
import Utilities.Err

irToBackend :: 𝑃 -> Err Program
irToBackend p' = do
  let p = simplify p'
  _ <- sanityCheck p
  -- _ <- homogeneous p
  -- _ <-
  --   multiGuard
  --     [ (not (stratified p), "Program is not stratified")
  --     ]
  (fv, ts) <- fvs p
  let ls = loops p
  let is = ifs p
  let k = getCaps p
  let ps = getProcs k p
  let chops = noloopPsChanInsns p
  let prog = wholeEncoding fv ts k ps chops is ls
  return prog
