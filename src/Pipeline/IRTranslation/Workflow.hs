module Pipeline.IRTranslation.Workflow (irToBackend) where

import IR.Ast
-- import IR.Homogeneity (homogeneous)
import IR.SanityCheck (sanityCheck)
import IR.Simplifier (simplify)
-- import IR.Stratification (stratified)
import Pipeline.IRTranslation.Clauses.CommPrecondition
import Pipeline.IRTranslation.Clauses.WgPrecondition
import Pipeline.IRTranslation.Clauses.Postcondition (postcondition)
import Pipeline.IRTranslation.Close
import Pipeline.IRTranslation.Encoding
import Pipeline.IRTranslation.Context.Capacity (caps)
import Pipeline.IRTranslation.Context.Reachability (reachability)
import Pipeline.IRTranslation.Context.TypeInference (typesAndFvs)
import Pipeline.IRTranslation.Context.WaitGroups (wgnames)
import Pipeline.IRTranslation.Processes (procs)
import Pipeline.IRTranslation.Meta.Meta
import Utilities.Err

-- | Convert IR program to back-end program. May fail.
irToBackend :: 𝑃 -> Err Encoding
irToBackend p' = do
  let p = simplify p'
  let (𝑃 defs _) = p
  _ <- sanityCheck p
  -- _ <- homogeneous p
  -- _ <-
  --   multiGuard
  --     [ (not (stratified p), "Program is not stratified")
  --     ]
  (𝛾, ts) <- typesAndFvs p
  let 𝜅 = caps p
  let 𝜓 = reachability p
  let 𝜉 = procs 𝜅 p
  let 𝓂 = getSummaries p
  return $ Encoding
    { prog = p,
      conditions = 𝜓,
      capacities = 𝜅,
      waitgroups = wgnames defs,
      typeenv = 𝛾,
      typevars = ts,
      processes = 𝜉,
      summaries = 𝓂,
      comprojection = projectedCommunication 𝜓 𝓂,
      wgprojection = projectedConcurrency 𝜓 𝓂,
      closes = closingChannels p,
      post = postcondition 𝜓 𝜉 (gs 𝓂)
    }
