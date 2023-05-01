module Pipeline.Translation.Workflow where

import IR.Ast (Prog)
import Pipeline.Translation.AlphaConversion (alphaConvert)
import Pipeline.Translation.CallgraphOk (noRecursion)
import Pipeline.Translation.PromelaToIR (getIR)
import Pipeline.Translation.SyntaxOk (allowed)
import Promela.Ast (Spec)
import Utilities.Err

promelaToIR :: Spec -> Err Prog
promelaToIR p = do
  let p' = alphaConvert p
  _ <- noRecursion p'
  _ <- allowed p'
  prog <- getIR p
  return prog
