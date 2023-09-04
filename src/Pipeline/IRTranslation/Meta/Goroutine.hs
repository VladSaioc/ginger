module Pipeline.IRTranslation.Meta.Goroutine where

import Pipeline.IRTranslation.Utilities

-- | Meta(data) about a goroutine spawning instruction.
data 𝒢 = 𝒢
  { -- | Process ID of return statement.
    gP :: P,
    -- | Process ID of child goroutine.
    gP' :: P,
    -- | Program point
    g𝑛 :: 𝑁
  } deriving Eq

instance Show 𝒢 where
  -- PID: return 𝑛
  show 𝒢 {gP = p, gP' = p', g𝑛 = 𝑛} = unwords [show p ++ ":", "go {", show 𝑛, ":", show p',"}"]
