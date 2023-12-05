module Pipeline.IRTranslation.Encoding where

import Data.Maybe qualified as Mb
import Data.Map qualified as M
import Data.Set qualified as S

import Backend.Ast
import Backend.Utilities
import Backend.Simplifier (eSimplify)
import IR.Ast qualified as I
import IR.Utilities
import Pipeline.IRTranslation.Meta.CommOp
import Pipeline.IRTranslation.Meta.Meta
import Pipeline.IRTranslation.Meta.WgOp
import Pipeline.IRTranslation.Utilities
import Utilities.Collection

{- | An abstract representation of the encoding.
-}
data Encoding = Encoding
  { -- | Original VIRGo program
    prog :: I.𝑃,
    -- | Reachability conditions of program points in the encoding.
    conditions :: 𝛹,
    -- | Channel capacities
    capacities :: 𝛫,
    -- | WaitGroups
    waitgroups :: 𝑊,
    -- | Type environment for concurrency parameters
    typeenv :: 𝛤,
    -- | Type variables required by polymorphic concurrency parameters
    typevars :: [Type],
    -- | Process encodings
    processes :: 𝛯,
    -- | High-level syntax summaries
    summaries :: ℳ,
    -- | Expressions encoding projected number of communication operations.
    comprojection :: 𝐶 ↦ (CommOpType ↦ Exp),
    -- | Expression encoding projected number of WaitGroup operations.
    wgprojection :: 𝐶 ↦ (WgOpType ↦ Exp),
    -- | Closed channels
    closes :: S.Set 𝐶,
    -- | Per-process postcondition
    post :: Exp
  }

-- | Get 'balanced-flow' precondition from the encoding.
balancedFlowPre :: Encoding -> Exp
balancedFlowPre Encoding { capacities = 𝜅, comprojection = p, wgprojection = w } =
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
       prw os = (os M.! A) :== (0 #)
    in ((M.elems (M.mapWithKey prc p) ++ M.elems (M.map prw w)) ...⋀)

-- | Checks whether there are any channels without send operations.
-- If there are no send operations and partial deadlocks are considered guaranteed,
-- likely the channel escapes the fragment scope, or is used as a free variable in a thunk,
-- (i.e. we are dealing with a false positive).
noSendsFound :: Encoding -> Bool
noSendsFound Encoding { comprojection = p } = any (\os -> eSimplify (os M.! S) == (0 #)) (M.elems p)

-- | Checks whether there are any channels without receive operations.
-- If there are no receive operations and partial deadlocks are considered guaranteed,
-- likely the channel escapes the fragment scope, or is used as a free variable in a thunk,
-- (i.e. we are dealing with a false positive).
noReceivesFound :: Encoding -> Bool
noReceivesFound Encoding { comprojection = p } = any (\os -> eSimplify (os M.! R) == (0 #)) (M.elems p)
