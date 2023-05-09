module IR.Simplifier (simplify) where

import IR.Ast

(#) :: Int -> Exp
(#) = Const

simplify :: Prog -> Prog
simplify (Prog cs ps) = Prog (map cOptimize cs) (map sOptimize ps)

cOptimize :: Chan -> Chan
cOptimize (Chan c e) = Chan c (eOptimize e)

sOptimize :: Stmt -> Stmt
sOptimize = \case
  -- skip; S ==> S
  Seq Skip s -> sOptimize s
  -- S; skip ==> S
  Seq s Skip -> sOptimize s
  Seq s1 s2 -> Seq (sOptimize s1) (sOptimize s2)
  -- for (x : e1 .. e2) {} ==> skip
  For _ _ _ [] -> Skip
  For x e1 e2 os -> For x (eOptimize e1) (eOptimize e2) os
  s -> s

eOptimize :: Exp -> Exp
eOptimize pe =
  let bin c e1 e2 = c (eOptimize e1) (eOptimize e2)
      e' = case pe of
        -- n1 + n2 ==> n
        Plus (Const n1) (Const n2) -> ((n1 + n2) #)
        -- e + 0 ==> e
        Plus e (Const 0) -> eOptimize e
        -- 0 + e ==> e
        Plus (Const 0) e -> eOptimize e
        Plus e1 e2 -> bin Plus e1 e2
        -- n1 - n2 ==> n
        Minus (Const n1) (Const n2) -> ((n1 - n2) #)
        -- e - 0 ==> e
        Minus e (Const 0) -> eOptimize e
        -- e - e ==> 0
        Minus e1 e2 ->
          if e1 == e2
            then (0 #)
            else bin Minus e1 e2
        -- n1 * n2 ==> n
        Mult (Const n1) (Const n2) -> ((n1 * n2) #)
        -- 1 * e ==> e
        Mult (Const 1) e -> eOptimize e
        -- e * 1 ==> e
        Mult e (Const 1) -> eOptimize e
        -- e * 0 ==> 0
        Mult _ (Const 0) -> (0 #)
        -- 0 * e ==> e
        Mult (Const 0) _ -> (0 #)
        Mult e1 e2 -> bin Mult e1 e2
        -- e / 1 ==> e
        Div e (Const 1) -> e
        Div e1 e2 -> bin Div e1 e2
        e -> e
   in if pe == e'
        then e'
        else eOptimize e'
