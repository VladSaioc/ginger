module IR.Stratification (stratified) where

import Data.List qualified as L
import IR.Ast
import IR.ChanString
import Utilities.General

stratified :: Prog -> Bool
stratified (Prog _ gos) =
  let chs = do results (Prelude.map getChanStrStmt gos)
   in all (\c -> all (congruent c) chs) chs

getChanStrAtomic :: Op -> String
getChanStrAtomic = \case
  Send c -> c
  Recv c -> c

getChanStrStmt :: Stmt -> Maybe [String]
getChanStrStmt = \case
  Seq s1 s2 -> do
    cs1 <- getChanStrStmt s1
    cs2 <- getChanStrStmt s2
    Just (cs1 ++ cs2)
  For _ _ _ s ->
    let cs = canonicalForm (L.map getChanStrAtomic s)
     in if L.length cs == 1
          then return cs
          else Nothing
  Atomic o -> return [getChanStrAtomic o]
  Skip -> return []
