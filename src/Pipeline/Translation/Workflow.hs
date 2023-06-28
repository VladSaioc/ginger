module Pipeline.Translation.Workflow where

import IR.Ast (Prog)
import IR.Simplifier (simplify)
import Pipeline.Sanity.CallgraphOk (noRecursion)
import Pipeline.Sanity.PromelaAllowed (allowed)
import Pipeline.Translation.AlphaConversion (alphaConvert)
import Pipeline.Translation.PromelaToIR (getIR)
import Promela.Ast (Spec)
import Utilities.Err

promelaToIR :: Spec -> Err Prog
promelaToIR p = do
  let p' = alphaConvert p
  _ <- noRecursion p'
  _ <- allowed p'
  ir <- getIR p'
  return $ simplify ir
