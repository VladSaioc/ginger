module Go.Simplifier where

import Data.Bifunctor

import Go.Ast
import Go.Utilities
import Utilities.Position

-- | Simplifies a program by recursively simplifying its statements.
simplify :: Prog -> Prog
simplify p@(Prog ss) =
  let simpl = Prog $ simplifyStatements ss
   in if simpl == p
        then simpl
        else simplify simpl

-- | Simplifies a list of statements by applying simplifications to each statement.
simplifyStatements :: [Pos Stmt] -> [Pos Stmt]
simplifyStatements = \case
  [] -> []
  Pos p s : ss ->
    let pos = Pos p
        un c ss' = c (simplifyStatements ss')
        bin c ss1 ss2 = c (simplifyStatements ss1) (simplifyStatements ss2)
     in case s of
          -- Close statement: No change.
          Close c -> pos (Close c) : simplifyStatements ss
          -- As statement: No change.
          As x e -> pos (As x e) : simplifyStatements ss
          -- Declaration statement: No change.
          Decl x e -> pos (Decl x e) : simplifyStatements ss
          -- Channel declaration statement: No change.
          Chan c e -> pos (Chan c e) : simplifyStatements ss
          -- WaitGroup declaration statement: No change.
          Wgdef w -> pos (Wgdef w) : simplifyStatements ss
          -- WaitGroup add: No change.
          Add e w -> pos (Add e w) : simplifyStatements ss
          -- WaitGroup wait: No change.
          Wait w -> pos (Wait w) : simplifyStatements ss
          -- Channel operation statement: No change.
          Atomic o -> pos (Atomic o) : simplifyStatements ss
          -- Skip statement: Remove redundant Skip.
          Skip -> simplifyStatements ss
          -- Return statement: Simplify to just Return.
          Return -> [Pos p Return]
          -- Break statement: Simplify to just Break.
          Break -> [Pos p Break]
          -- Continue statement: Simplify to just Continue.
          Continue -> [Pos p Continue]
          -- Block statement: Flatten nested blocks.
          Block ss' -> simplifyStatements $ ss' ++ ss
          -- If-else statement:
          -- - Remove empty if-else.
          -- - Simplify if with true condition.
          -- - Simplify if with false condition.
          If _ [] [] -> simplifyStatements ss
          If CTrue ss' _ -> simplifyStatements $ ss' ++ ss
          If CFalse _ ss' -> simplifyStatements $ ss' ++ ss
          If e ss1 ss2 -> pos (bin (If e) ss1 ss2) : simplifyStatements ss
          -- Select statement:
          -- - Reduce empty select to itself. No continuation is possible.
          -- - Remove redundant empty non-blocking select.
          Select [] Nothing -> [Pos p $ Select [] Nothing]
          -- Inline blocking single send case select statements
          Select [(Pos p' (Send c), ss')] Nothing -> simplifyStatements (Pos p' (Atomic (Send c)) : ss' ++ ss)
          -- Inline blocking single receive case select statements
          Select [(Pos p' (Recv c), ss')] Nothing -> simplifyStatements (Pos p' (Atomic (Recv c)) : ss' ++ ss)
          -- Inline blocking single star case select statements
          Select [(Pos _ Star, ss')] Nothing -> simplifyStatements $ ss' ++ ss
          -- Inline default statement.
          Select [] (Just ss') -> simplifyStatements $ ss' ++ ss
          Select cs ds ->
            if not (relevantSelect s) && all (null . snd) cs && maybe True null ds
            -- Remove select statement if nothing interesting happens
            then simplifyStatements ss
            else
                let cs' = map (second simplifyStatements) cs
                    ds' = fmap simplifyStatements ds
                in pos (Select cs' ds') : simplifyStatements ss
          -- Go statement:
          -- - Remove redundant empty go.
          Go [] -> simplifyStatements ss
          Go ss' -> pos (un Go ss') : simplifyStatements ss
          -- For loop:
          -- - Remove redundant empty for loop.
          For _ _ _ _ [] -> simplifyStatements ss
          For x e1 e2 d ss' -> pos (un (For x e1 e2 d) ss') : simplifyStatements ss
          -- While loop:
          -- - Remove redundant empty while loop.
          While _ [] -> simplifyStatements ss
          While e ss' -> pos (un (While e) ss') : simplifyStatements ss
