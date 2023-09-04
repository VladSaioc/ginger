module Pipeline.IRTranslation.Meta.Loop where

import Backend.Ast
import Data.List (intercalate)
import Data.Map qualified as M
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