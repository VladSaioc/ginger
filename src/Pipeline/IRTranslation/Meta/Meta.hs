module Pipeline.IRTranslation.Meta.Meta where

import IR.Ast
import Pipeline.IRTranslation.Meta.Channel
import Pipeline.IRTranslation.Meta.Go
import Pipeline.IRTranslation.Meta.If
import Pipeline.IRTranslation.Meta.Loop
import Pipeline.IRTranslation.Meta.Return
import Pipeline.IRTranslation.Utilities
import Utilities.Collection

-- | All statement metadata in the program.
data ℳ = ℳ
  { -- | Channel operation metadata
    os :: P ↦ (𝐶 ↦ 𝒪s),
    -- | Go statement metadata
    gs :: [𝒢],
    -- | If statement metadata
    is :: [ℐ],
    -- | Loop metadata
    ls :: [ℒ],
    -- | Return statement metadata
    rs :: [ℛ]
  }

meta :: 𝛫 -> 𝑃 -> ℳ
meta 𝜅 p =
  ℳ
    {
      os = noloopPsChanInsns p,
      gs = gos p,
      is = ifs p,
      ls = loops p,
      rs = returns p
    }