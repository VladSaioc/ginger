module Pipeline.IRTranslation.Meta.If where

import Backend.Ast
import Pipeline.IRTranslation.Utilities
import Utilities.PrettyPrint

-- (Meta)data about conditional statements found in the program.
data ℐ = ℐ
  { -- Process
    iP :: P,
    -- If guard
    iGuardExp :: Exp,
    -- Guard program point
    iGuard :: P𝑛,
    -- Else program point
    iElse :: P𝑛,
    -- Exit program point
    iExit :: P𝑛
  }

instance Show ℐ where
  show ℐ {iP = p, iGuardExp = g, iGuard = n1, iElse = n2, iExit = n3} =
    multiline
      -- PID: for x (lo .. hi) <n₁ --> n₂>
      [ unwords
          [ show p ++ ":",
            "if",
            "(" ++ prettyPrint 0 g,
            "<" ++ show n1 ++ "-->" ++ show n2 ++ "-->" ++ show n3 ++ ">"
          ]
      ]