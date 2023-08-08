module Pipeline.IRTranslation.Meta.If where

import Backend.Ast
import Pipeline.IRTranslation.Utilities
import Utilities.PrettyPrint

-- (Meta)data about conditional statements found in the program.
data ℐ = ℐ
  { -- Process
    iP :: P,
    -- If guard
    iGuard :: Exp,
    -- Guard program point
    i𝑛 :: 𝑁,
    -- Else program point
    iElse :: 𝑁,
    -- Exit program point
    iExit :: 𝑁
  }

instance Show ℐ where
  show ℐ {iP = p, iGuard = g, i𝑛 = 𝑛₁, iElse = 𝑛₂, iExit = 𝑛₃} =
    multiline
      -- PID: for x (lo .. hi) <n₁ --> n₂>
      [ unwords
          [ show p ++ ":",
            show 𝑛₁ ++ ": if",
            "(" ++ prettyPrint 0 g ++ ")",
            "<" ++ "-->" ++ show 𝑛₂ ++ "; -->" ++ show 𝑛₃ ++ ">"
          ]
      ]