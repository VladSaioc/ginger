module Go.Simplifier where

import Data.Bifunctor
import Go.Ast
import Go.Utilities
import Utilities.Position

simplify :: Prog -> Prog
simplify p@(Prog ss) =
  let simpl = Prog $ simplifyStatements ss
   in if simpl == p
        then simpl
        else simplify simpl

simplifyStatements :: [Pos Stmt] -> [Pos Stmt]
simplifyStatements = \case
  [] -> []
  Pos p s : ss ->
    let pos = Pos p
        un c ss' = c (simplifyStatements ss')
        bin c ss1 ss2 = c (simplifyStatements ss1) (simplifyStatements ss2)
     in case s of
          Close c -> pos (Close c) : simplifyStatements ss
          As x e -> pos (As x e) : simplifyStatements ss
          Decl x e -> pos (Decl x e) : simplifyStatements ss
          Chan c e -> pos (Chan c e) : simplifyStatements ss
          Atomic o -> pos (Atomic o) : simplifyStatements ss
          Skip -> simplifyStatements ss
          Return -> [Pos p Return]
          Break -> [Pos p Break]
          Block ss' -> simplifyStatements $ ss' ++ ss
          If _ [] [] -> simplifyStatements ss
          If CTrue ss' _ -> simplifyStatements $ ss' ++ ss
          If CFalse _ ss' -> simplifyStatements $ ss' ++ ss
          If e ss1 ss2 -> pos (bin (If e) ss1 ss2) : simplifyStatements ss
          Select [] Nothing -> [Pos p $ Select [] Nothing]
          Select [] (Just ss') -> simplifyStatements $ ss' ++ ss
          Select cs ds ->
            if not (relevantSelect s) && all (null . snd) cs && maybe True null ds
            then []
            else
                let cs' = map (second simplifyStatements) cs
                    ds' = fmap simplifyStatements ds
                in pos (Select cs' ds') : simplifyStatements ss
          Go [] -> simplifyStatements ss
          Go ss' -> pos (un Go ss') : simplifyStatements ss
          For _ _ _ _ [] -> simplifyStatements ss
          For x e1 e2 d ss' -> pos (un (For x e1 e2 d) ss') : simplifyStatements ss
          While _ [] -> simplifyStatements ss
          While e ss' -> pos (un (While e) ss') : simplifyStatements ss
