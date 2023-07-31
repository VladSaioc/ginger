module IR.Simplifier (simplify) where

import IR.Ast

(#) :: Int -> 𝐸
(#) = Const

simplify :: 𝑃 -> 𝑃
simplify (𝑃 cs ps) = 𝑃 (map cOptimize cs) (map sOptimize ps)

cOptimize :: Chan -> Chan
cOptimize (Chan c e) = Chan c (eOptimize e)

sOptimize :: 𝑆 -> 𝑆
sOptimize = \case
  -- skip; S ==> S
  Seq Skip s -> sOptimize s
  -- S; skip ==> S
  Seq s Skip -> sOptimize s
  Seq s1 s2 -> Seq (sOptimize s1) (sOptimize s2)
  -- for (x : e1 .. e2) {} ==> skip
  For _ _ _ [] -> Skip
  For x e1 e2 os -> For x (eOptimize e1) (eOptimize e2) os
  -- if true then S1 else S2 ==> S1
  If BTrue s1 _ -> s1
  -- if false then S1 else S2 ==> S2
  If BFalse _ s2 -> s2
  If e s1 s2 ->
    let e' = eOptimize e
     in if e' /= e
          then sOptimize (If e' s1 s2)
          else If e' (sOptimize s1) (sOptimize s2)
  s -> s

eOptimize :: 𝐸 -> 𝐸
eOptimize pe =
  let bin c e1 e2 = c (eOptimize e1) (eOptimize e2)
      e' = case pe of
        -- n1 + n2 ==> n
        Const n1 :+ Const n2 -> ((n1 + n2) #)
        -- e + 0 ==> e
        e :+ Const 0 -> eOptimize e
        -- 0 + e ==> e
        Const 0 :+ e -> eOptimize e
        e1 :+ e2 -> bin (:+) e1 e2
        -- n1 - n2 ==> n
        Const n1 :- Const n2 -> ((n1 - n2) #)
        -- e - 0 ==> e
        e :- Const 0 -> eOptimize e
        -- e - e ==> 0
        e1 :- e2 -> if e1 == e2 then (0 #) else bin (:-) e1 e2
        -- n1 * n2 ==> n
        Const n1 :* Const n2 -> ((n1 * n2) #)
        -- 1 * e ==> e
        Const 1 :* e -> eOptimize e
        -- e * 1 ==> e
        e :* Const 1 -> eOptimize e
        -- e * 0 ==> 0
        _ :* Const 0 -> (0 #)
        -- 0 * e ==> e
        Const 0 :* _ -> (0 #)
        e1 :* e2 -> bin (:*) e1 e2
        -- e / 1 ==> e
        e :/ Const 1 -> e
        e1 :/ e2 -> bin (:/) e1 e2
        e -> e
   in if pe == e'
        then e'
        else eOptimize e'
