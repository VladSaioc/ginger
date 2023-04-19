module Pipeline.IRTranslation.ExtractChannels (getCaps) where

import Data.Map qualified as M
import IR.Ast
import Pipeline.IRTranslation.Exps (parseExp)
import Pipeline.IRTranslation.Utilities

getCaps :: Prog -> CapEnv
getCaps (Prog chs _) =
  let updateChanEnv env (Chan c e) = M.insert c (parseExp e) env
   in Prelude.foldl updateChanEnv M.empty chs
