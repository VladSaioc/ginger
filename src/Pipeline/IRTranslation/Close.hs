module Pipeline.IRTranslation.Close where

import Data.Set qualified as S

import IR.Ast
import Pipeline.IRTranslation.Meta.CommOp
import Pipeline.IRTranslation.Utilities

closingChannels :: 𝑃 -> S.Set 𝐶
closingChannels = programToCollection (const $ \case
  Close c -> S.singleton c
  _ -> S.empty)
