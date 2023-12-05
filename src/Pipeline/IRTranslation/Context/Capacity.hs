module Pipeline.IRTranslation.Context.Capacity (caps) where

import Data.Map qualified as M

import IR.Ast
import Pipeline.IRTranslation.Exps (parseExp)
import Pipeline.IRTranslation.Meta.CommOp

{- | Extract capacity expressions from channel definitions in
  IR program.
-}
caps :: 𝑃 -> 𝛫
caps (𝑃 chs _) =
  let updateChanEnv env = \case
        (Chan c e) -> M.insert c (parseExp e) env
        _ -> env
   in Prelude.foldl updateChanEnv M.empty chs
