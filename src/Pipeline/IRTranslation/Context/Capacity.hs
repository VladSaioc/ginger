module Pipeline.IRTranslation.Context.Capacity (caps) where

import Data.Map qualified as M

import IR.Ast
import Pipeline.IRTranslation.Exps (parseExp)
import Pipeline.IRTranslation.Summary.Chan
import Pipeline.IRTranslation.Utilities

{- | Extract capacity expressions from channel definitions in a VIRGo program.
-}
caps :: 𝑆 -> 𝛫
caps = foldStatement M.union processChans

processChans :: 𝛬 -> 𝑆 -> 𝛫
processChans _ = \case
  Def (Chan c e) -> M.singleton c (parseExp e)
  _ -> M.empty
