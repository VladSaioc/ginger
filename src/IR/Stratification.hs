module IR.Stratification (stratified) where

import Data.List qualified as L
import IR.Ast
import IR.ChanString
import IR.Utilities

stratified :: 𝑃 -> Bool
stratified (𝑃 _ gos) =
  let mcss = getChanStrStmt gos
   in case mcss of
        Just css -> all (\c -> all (congruent c) css) css
        Nothing -> False

getChanStrStmt :: 𝑆 -> Maybe [String]
getChanStrStmt = \case
  Go {} -> Nothing
  If {} -> Nothing
  Return {} -> Nothing
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
