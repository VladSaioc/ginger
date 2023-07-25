module Go.Simplifier where

import Go.Ast

-- simplify :: Prog -> Prog
-- simplify p@(Prog ss) =
--   let simpl = Prog $ simplifyStatements ss
--    in if simpl == p
--         then simpl
--         else simplify simpl

-- simplifyStatements :: [Stmt] -> [Stmt]
-- simplifyStatements = \case
--   [] -> []
--   s : ss -> case s of
--     Skip -> simplifyStatements ss
--     Return -> [Return]
--     Break -> [Break]
--     Block ss' -> simplifyStatements $ ss' ++ ss
--     If CTrue ss' _ -> simplifyStatements $ ss' ++ ss
--     If CFalse _ ss' -> simplifyStatements $ ss' ++ ss
--     -- If e ss1 ss2 ->

--     Select [] Nothing -> [Select [] Nothing]
--     Select [] (Just ss') -> simplifyStatements $ ss' ++ ss
--     Go [] -> simplifyStatements ss
