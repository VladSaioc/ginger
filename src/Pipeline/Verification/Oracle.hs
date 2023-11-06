module Pipeline.Verification.Oracle where

import Backend.Ast
import Backend.Utilities
import Pipeline.IRTranslation.Encoding
import Pipeline.IRTranslation.Meta.Meta
import Pipeline.IRTranslation.Clauses.Postcondition (postcondition)
import Pipeline.IRTranslation.Clauses.CapPrecondition (capPreconditions)
import Backend.Simplifier (eSimplify)

-- | Oracles contextualize how verification is performed. They are provided with pre/post condition construction.
data Oracle = Oracle {
  -- | Oracle name
  oname :: String,
  -- | A short name for the oracle, that can be added to e.g., a file name.
  shortName :: String,
  -- | A description of the oracle.
  description :: String,
  -- | Message to print if verification succeeds.
  successMessage :: Encoding -> String,
  -- | Encoding-to-precondition factory.
  makePrecondition :: Encoding -> Exp,
  -- | Encoding-to-postcondition factory.
  makePostcondition :: Encoding -> Exp
}

-- | Wrap postcondition construction over an entire encoding.
encodingToPostcondition :: Encoding -> Exp
encodingToPostcondition Encoding { conditions = 𝜓, processes = 𝜉, summaries = ℳ { gs } } = postcondition 𝜓 𝜉 gs

-- | The trivial oracle. Verification is carried out under the weakest precondition (true).
-- If verification succeeds, partial deadlock freedom is demonstrated for all inputs.
trivial :: Oracle
trivial = Oracle {
  oname = "Trivial",
  shortName = "trivial",
  description = unlines [
    "Running the trivial oracle. The precondition is assumed to be `true`.",
    "If the encoding verifies, the model is partial deadlock-free for all concurrency parameter values."
  ],
  successMessage = const "The program has been validated as partial deadlock-free for all inputs.",
  -- Precondition is the weakest possible
  makePrecondition = const (True ?),
  -- Postcondition only states that all processes should terminate.
  makePostcondition = encodingToPostcondition
}

-- | The balanced-flow oracle (weakest-precondition variant). Verification is carried out under the "balanced-flow" precondition in the WP position.
-- For a program P, the balanced-flow precondition is:
-- > ∀ c ∈ channels(P). receives(c) ≤ sends(c) ≤ receives(c) + κ(c)
balancedFlowWP :: Oracle
balancedFlowWP = Oracle {
  oname = "Balanced Flow (weakest-precondition variant)",
  shortName = "balanced-flow-wp",
  description = unlines [
    "Running the 'balanced-flow' oracle in the weakest-precondition position. The precondition states that for every receive operation there should be a corresponding, and that the number of sends must not exceed the number of receives and the channel capacity.",
    "The precondition is placed in the weakest-precondition position.",
    "If the encoding verifies, the model is partial deadlock-free if and only if the concurrency parameters satisfy the precondition."
  ],
  successMessage = \encoding@Encoding { capacities = κ } ->
    unlines [
      "Constraints from capacities:",
      "\t" ++ show (eSimplify (capPreconditions κ ...⋀)),
      "The program is partial deadlock-free if and only if:",
      "\t" ++ show (eSimplify $ pre encoding)
    ],
  -- Precondition is incorporated in the postcondition under equivalence i.e.,
  -- Partial deadlock freedom implies the precondition, and vice-versa.
  -- If satisfied, the precondition is also proven to be the weakest (modulo scheduling choices).
  makePrecondition = const (True ?),
  makePostcondition = \encoding -> pre encoding :<==> encodingToPostcondition encoding
}

-- | The balanced-flow oracle. Verification is carried out under the "balanced-flow" precondition.
-- For a program P, the balanced-flow precondition is:
-- > ∀ c ∈ channels(P). receives(c) ≤ sends(c) ≤ receives(c) + κ(c)
balancedFlow :: Oracle
balancedFlow = Oracle {
  oname = "Balanced Flow",
  shortName = "balanced-flow",
  description = unlines [
    "Running the 'balanced-flow' oracle. The precondition states that for every receive operation there should be a corresponding, and that the number of sends must not exceed the number of receives and the channel capacity.",
    "If the encoding verifies, the model is partial deadlock-free if the concurrency parameters satisfy the precondition.",
    "However, the converse need not be true."
  ],
  successMessage = \encoding@Encoding { capacities = κ } ->
    unlines [
      "Constraints from capacities:",
      "\t" ++ show (eSimplify (capPreconditions κ ...⋀)),
      "The program is partial deadlock-free if:",
      "\t" ++ show (eSimplify $ pre encoding),
      "This condition may be more restrictive than necessary."
    ],
  -- Precondition
  makePrecondition = pre,
  makePostcondition = encodingToPostcondition
}
