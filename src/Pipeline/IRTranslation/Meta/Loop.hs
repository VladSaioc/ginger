module Pipeline.IRTranslation.Meta.Loop where

import Backend.Ast
import Data.List (intercalate)
import Data.Map qualified as M
import Data.Maybe (fromMaybe)
import IR.Ast
import IR.Utilities
import Pipeline.IRTranslation.Exps (parseExp)
import Pipeline.IRTranslation.Meta.Channel
import Pipeline.IRTranslation.Utilities
import Utilities.Collection
import Utilities.PrettyPrint

-- | (Meta)data about loops found in the program.
data ℒ = ℒ
  { -- | Process of loop
    lP :: P,
    -- | Index variable
    l𝑋 :: String,
    -- | Guard program point
    l𝑛 :: 𝑁,
    -- | Exit program point
    lExit :: 𝑁,
    -- | Lower bound
    lower :: Exp,
    -- | Upper bound
    upper :: Exp,
    -- | Channel operations in the loop (indexed by channel name)
    l𝒪s :: 𝐶 ↦ 𝒪s
  } deriving Eq

instance Show ℒ where
  show ℒ {lP = p, l𝑋 = x, l𝑛 = 𝑛₁, lExit = 𝑛₂, lower, upper, l𝒪s} =
    multiline
      -- PID: for x (lo .. hi) <𝑛₁ --> 𝑛₂>
      [ unwords
          [ show p ++ ":",
            unwords ["for ", x, " (" ++ prettyPrint 0 lower ++ ".." ++ prettyPrint 0 upper ++ ")"],
            "<" ++ show 𝑛₁ ++ "-->" ++ show 𝑛₂ ++ ">"
          ],
        intercalate ", " (M.elems (M.map show l𝒪s))
      ]

-- | Collect all loops found in the program.
-- Assume that all loops are initially guarded by true.
loops :: 𝑃 -> [ℒ]
loops = programToCollection processLoops

-- | Collect all loops found in a process.
processLoops :: 𝛬 -> 𝑆 -> [ℒ]
processLoops 𝛬 { p, 𝑛 } = \case
  For x e1 e2 os ->
    let -- Decorate loop index with process id.
        x' = p % x
        -- Gather all channel operations and the next available program
        -- point.
        (chops, 𝑛') = chanOps p (𝑛 + 1) os
        l =
          ℒ
            { -- Loop process
              lP = p,
              -- Loop index
              l𝑋 = x',
              -- Guard is at the entry program point
              l𝑛 = 𝑛,
              -- Exit point is after the increment operation program point,
              -- which is the exit program point of the loop body
              lExit = 𝑛' + 1,
              -- Parse lower bound expression
              lower = parseExp e1,
              -- Parse upper bound expression
              upper = parseExp e2,
              -- Add channel operations
              l𝒪s = chops
            }
     in [l]
  _ -> []
     

-- | Collect all channel operations in a loop.
-- Relevant information includes: channel name, program point
-- and direction.
chanOps :: P -> 𝑁 -> [Op] -> (𝐶 ↦ 𝒪s, 𝑁)
chanOps p 𝑛 =
  let addOp (chops, 𝑛') op =
        let -- Get channel name and direction
            (c, d) = (chName op, chDir op)
            -- Get program counters for channel operations
            𝑛s = fromMaybe M.empty (M.lookup c chops)
            -- Get list of channel operations for the direction
            dpcs = fromMaybe [] (M.lookup d 𝑛s)
            ch =
              𝒪
                { oP = p,
                  o𝐶 = c,
                  oDir = d,
                  o𝑛 = 𝑛'
                }
            pcs' = M.insert d (ch : dpcs) 𝑛s
         in (M.insert c pcs' chops, 𝑛' + ppOffset op)
   in Prelude.foldl addOp (M.empty, 𝑛)
