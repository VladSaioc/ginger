module Pipeline.IRTranslation.Meta.Return where

import Backend.Ast
import Pipeline.IRTranslation.Utilities

-- Meta(data) about a return instruction.
data ℛ = ℛ
  { -- Process ID
    rP :: P,
    -- Program point
    r𝑛 :: N,
    -- Process exit point
    rexit :: N,
    -- Path conditions guarding the return statement
    rPathexp :: Exp
  }

instance Show ℛ where
  -- PID: return 𝑛
  show ℛ {rP = p, r𝑛 = 𝑛} = unwords [show p ++ ":", "return", show 𝑛]
