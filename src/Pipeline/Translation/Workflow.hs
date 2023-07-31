module Pipeline.Translation.Workflow where

import Debug.Trace (trace)
import Go.GoForCommute (goForCommute)
import IR.Ast (𝑃)
import IR.Simplifier (simplify)
import Pipeline.Sanity.CallgraphOk (noRecursion)
import Pipeline.Sanity.GoAllowed (allowed)
import Pipeline.Translation.AlphaConversion (alphaConvert)
import Pipeline.Translation.GoToIR (getIR)
import Pipeline.Translation.PromelaToGo (getGo)
import Promela.Ast (Spec)
import Utilities.Err

promelaToIR :: Spec -> Err 𝑃
promelaToIR p = do
  let p' = alphaConvert p
  _ <- noRecursion p'
  g <- getGo p'
  let g1 = goForCommute g
  _ <- allowed g1
  _ <- trace (unlines ["", "Go represention of program:", "", show g1]) (return ())
  ir <- getIR g1
  return $ simplify ir
