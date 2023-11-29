module Pipeline.IRTranslation.Meta.Meta where

import IR.Ast
import Pipeline.IRTranslation.Meta.CommOp
import Pipeline.IRTranslation.Meta.Go
import Pipeline.IRTranslation.Meta.If
import Pipeline.IRTranslation.Meta.Loop
import Pipeline.IRTranslation.Meta.Return
import Pipeline.IRTranslation.Meta.WgOp
import Pipeline.IRTranslation.Utilities
import Utilities.Collection

-- | All statement summaries in the program.
data ℳ = ℳ
  { -- | Channel outside-loop operation summaries
    os :: P ↦ (𝐶 ↦ 𝒪s),
    -- | WaitGroup outside-loop operation summaries
    ws :: P ↦ (𝐶 ↦ 𝒲s),
    -- | Go statement summaries
    gs :: [𝒢],
    -- | If statement summaries
    is :: [ℐ],
    -- | Loop summaries
    ls :: [ℒ],
    -- | Return statement summaries
    rs :: [ℛ]
  }

getSummaries :: 𝑃 -> ℳ
getSummaries p =
  ℳ
    {
      os = noloopPsChanInsns p,
      ws = noloopPsWgInsns p,
      gs = gos p,
      is = ifs p,
      ls = loops p,
      rs = returns p
    }
