module IR.Stratification (stratified) where

import Data.List qualified as L
import IR.Ast
import IR.ChanString
import IR.Utilities
import Utilities.General

stratified :: Prog -> Bool
stratified (Prog _ gos) =
  let mcss = do results (Prelude.map getChanStrStmt gos)
  in case mcss of
    Just css -> all (\c -> all (congruent c) css) css
    Nothing -> False

getChanStrStmt :: Stmt -> Maybe [String]
getChanStrStmt = \case
  Seq s1 s2 -> do
    cs1 <- getChanStrStmt s1
    cs2 <- getChanStrStmt s2
    Just (cs1 ++ cs2)
  For _ _ _ s ->
    let cs = canonicalForm (L.map chName s)
     in if L.length cs == 1
          then return cs
          else Nothing
  Atomic o -> return [chName o]
  Skip -> return []
