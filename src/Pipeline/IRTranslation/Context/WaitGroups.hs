module Pipeline.IRTranslation.Context.WaitGroups (wgnames) where

import Data.Set qualified as S

import IR.Ast
import Pipeline.IRTranslation.Utilities

wgnames :: 𝑆 -> S.Set String
wgnames = programToCollection $ const (\case
  Def (Wg w) -> S.singleton w
  _ -> S.empty)
