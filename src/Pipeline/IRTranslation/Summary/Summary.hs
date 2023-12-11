module Pipeline.IRTranslation.Summary.Summary where

import IR.Ast
import Pipeline.IRTranslation.Summary.Chan
import Pipeline.IRTranslation.Summary.CommOp
import Pipeline.IRTranslation.Summary.Go
import Pipeline.IRTranslation.Summary.If
import Pipeline.IRTranslation.Summary.Loop
import Pipeline.IRTranslation.Summary.Return
import Pipeline.IRTranslation.Summary.WgOp
import Pipeline.IRTranslation.Utilities
import Utilities.Collection

-- | All statement summaries in the program.
data ℳ = ℳ
  { -- | Channel definition summaries
    cs :: [𝒞],
    -- | Channel outside-loop operation summaries
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

getSummaries :: 𝑆 -> ℳ
getSummaries p =
  ℳ
    {
      cs = chandefs p,
      os = noloopPsChanInsns p,
      ws = noloopPsWgInsns p,
      gs = gos p,
      is = ifs p,
      ls = loops p,
      rs = returns p
    }
