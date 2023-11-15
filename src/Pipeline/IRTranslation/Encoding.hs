module Pipeline.IRTranslation.Encoding where

import Backend.Ast
import Backend.Utilities
import Backend.Simplifier (eSimplify)
import Data.Maybe qualified as Mb
import Data.Map qualified as M
import IR.Ast qualified as S
import IR.Utilities
import Pipeline.IRTranslation.Meta.Channel
import Pipeline.IRTranslation.Meta.Meta
import Pipeline.IRTranslation.Utilities
import Utilities.Collection

{- | An abstract representation of the encoding.
-}
data Encoding = Encoding
  { -- | Original VIRGo program
    prog :: S.𝑃,
    -- | Reachability conditions of program points in the encoding.
    conditions :: 𝛹,
    -- | Channel capacities
    capacities :: 𝛫,
    -- | Type environment for concurrency parameters
    typeenv :: 𝛤,
    -- | Type variables required by polymorphic concurrency parameters
    typevars :: [Type],
    -- | Process encodings
    processes :: 𝛯,
    -- | High-level syntax summaries
    summaries :: ℳ,
    -- | Expressions encoding projected number of communication operations.
    comprojection :: 𝐶 ↦ (OpDir ↦ Exp),
    -- | Per-process postcondition
    post :: Exp
  }

-- | Get 'balanced-flow' precondition from the encoding.
pre :: Encoding -> Exp
pre Encoding { capacities = 𝜅, comprojection = p } =
   let prc c os =
         let -- Get channel capacity expression.
            k = Mb.fromJust (M.lookup c 𝜅)
            -- Get projected number of sends
            sends = os M.! S
            -- Get projected number of receives
            recvs = os M.! R
            -- Construct receive unblock precondition.
            -- Receives unblock if there are more sends.
            rcvsUnblock = recvs :<= sends
            -- Construct send unblock precondition.
            -- Sends unblock if there are more receive operations and
            -- capacity combined.
            sndsUnblock = sends :<= (recvs :+ k)
         in rcvsUnblock :&& sndsUnblock
    in ((M.elems $ M.mapWithKey prc p)  ...⋀)

-- | noSendsFound checks whether there are any channels without send operations.
-- If there are no send operations and partial deadlocks are considered guaranteed,
-- likely the channel escapes the fragment scope, or is used as a free variable in a thunk,
-- (i.e. we are dealing with a false positive).
noSendsFound :: Encoding -> Bool
noSendsFound Encoding { comprojection = p } = any (\os -> eSimplify (os M.! S) == (0 #)) (M.elems p)

-- | noReceivesFound checks whether there are any channels without receive operations.
-- If there are no receive operations and partial deadlocks are considered guaranteed,
-- likely the channel escapes the fragment scope, or is used as a free variable in a thunk,
-- (i.e. we are dealing with a false positive).
noReceivesFound :: Encoding -> Bool
noReceivesFound Encoding { comprojection = p } = any (\os -> eSimplify (os M.! R) == (0 #)) (M.elems p)
