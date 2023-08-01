module Pipeline.IRTranslation.Meta.Loop where

import Backend.Ast
import Data.List (intercalate)
import Data.Map qualified as M
import Pipeline.IRTranslation.Meta.Channel
import Pipeline.IRTranslation.Utilities
import Utilities.PrettyPrint

-- (Meta)data about loops found in the program.
data ℒ = ℒ
  { -- Process
    lP :: P,
    -- Index variable
    l𝑋 :: String,
    -- Guard program point
    lGuard :: P𝑛,
    -- Exit program point
    lExit :: P𝑛,
    -- Lower bound
    lower :: Exp,
    -- Upper bound
    upper :: Exp,
    -- Channel operations in the loop (indexed by channel name)
    l𝒪s :: 𝐶 ↦ 𝒪s,
    -- Path conditions guarding the loop
    lPathexp :: Exp
  }

instance Show ℒ where
  show ℒ {lP = p, l𝑋 = x, lGuard = n1, lExit = n2, lower, upper, l𝒪s} =
    multiline
      -- PID: for x (lo .. hi) <n₁ --> n₂>
      [ unwords
          [ show p ++ ":",
            unwords ["for ", x, " (" ++ prettyPrint 0 lower ++ ".." ++ prettyPrint 0 upper ++ ")"],
            "<" ++ show n1 ++ "-->" ++ show n2 ++ ">"
          ],
        intercalate ", " (M.elems (M.map show l𝒪s))
      ]