module Pipeline.IRTranslation.Context.WaitGroups (wgnames) where


import Data.Set qualified as S
import IR.Ast

wgnames :: Foldable t => t 𝐷 -> S.Set String
wgnames defs =
  let wgfromDef = \case
        (Chan {}) -> []
        (Wg w) -> [w]
   in S.fromList $ concatMap wgfromDef defs
