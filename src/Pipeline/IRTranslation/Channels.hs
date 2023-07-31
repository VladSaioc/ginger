module Pipeline.IRTranslation.Channels (getCaps) where

import Data.Map qualified as M
import IR.Ast
import Pipeline.IRTranslation.Exps (parseExp)
import Pipeline.IRTranslation.Meta.Channel

{- Extract capacity expressions from channel definitions in
  IR program.
-}
getCaps :: 𝑃 -> K
getCaps (𝑃 chs _) =
  let updateChanEnv env (Chan c e) = M.insert c (parseExp e) env
   in Prelude.foldl updateChanEnv M.empty chs
