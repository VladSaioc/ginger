module Pipeline.IRTranslation.Workflow (irToBackend) where

import Backend.Ast (Program)
import IR.Ast (Prog)
import IR.Homogeneity (homogeneous)
import IR.SanityCheck (asyncCheck, sanityCheck)
import IR.Simplifier (simplify)
import IR.Stratification (stratified)
import Pipeline.IRTranslation.Boilerplate (wholeEncoding)
import Pipeline.IRTranslation.ChInstructions (noloopPsChanInsns)
import Pipeline.IRTranslation.Channels (getCaps)
import Pipeline.IRTranslation.FreeVars (fvs)
import Pipeline.IRTranslation.Loop (loops)
import Pipeline.IRTranslation.Processes (getProcs)
import Utilities.Err

irToBackend :: Prog -> Err Program
irToBackend p' = do
  let p = simplify p'
  _ <- sanityCheck p
  _ <- asyncCheck p
  _ <- homogeneous p
  _ <-
    multiGuard
      [ (not (stratified p), "Program is not stratified")
      ]
  let fv = fvs p
  let ls = loops p
  let k = getCaps p
  let ps = getProcs k p
  let chops = noloopPsChanInsns p
  let prog = wholeEncoding fv k ps chops ls
  return prog
