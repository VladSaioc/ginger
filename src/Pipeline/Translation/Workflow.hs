module Pipeline.Translation.Workflow where

import Debug.Trace (trace)
import Go.GoForCommute (goForCommute)
import Go.Simplifier qualified as S (simplify)
import Go.ZipCases (zipCases)
import IR.Ast (𝑃)
import IR.Simplifier qualified as S' (simplify)
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
  let g1 = zipCases g
  let g2 = S.simplify g1
  let g' = goForCommute g2
  _ <- trace (unlines ["", "Go represention of program:", "", show g']) (return ())
  _ <- allowed g'
  ir <- getIR g'
  return $ S'.simplify ir
