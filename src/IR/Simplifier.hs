module IR.Simplifier (simplify) where

import IR.Ast

(#) :: Int -> 𝐸
(#) = Const

-- | Simplify IR statements.
simplify :: 𝑃 -> 𝑃
simplify (𝑃 cs s) = 𝑃 (map cOptimize cs) (sSimplify s)

-- | Simplify IR channel definitions.
cOptimize :: Chan -> Chan
cOptimize (Chan c e) = Chan c (eSimplify e)

-- | Simplify IR statements.
sSimplify :: 𝑆 -> 𝑆
sSimplify s =
  let s' = case s of
        -- return; S ==> return
        Seq Return _ -> Return
        -- skip; S ==> S
        Seq Skip s2 -> sSimplify s2
        -- S; skip ==> S
        Seq s1 Skip -> sSimplify s1
        Seq s1 s2 -> Seq (sSimplify s1) (sSimplify s2)
        -- for (x : e1 .. e2) {} ==> skip
        For _ _ _ [] -> Skip
        For x e1 e2 os -> For x (eSimplify e1) (eSimplify e2) os
        -- if true then S1 else S2 ==> S1
        If BTrue s1 _ -> s1
        -- if false then S1 else S2 ==> S2
        If BFalse _ s2 -> s2
        If e s1 s2 ->
          let e' = eSimplify e
           in if e' /= e
                then sSimplify (If e' s1 s2)
                else If e' (sSimplify s1) (sSimplify s2)
        -- go { skip } ==> skip
        Go Skip -> Skip
        Go s1 -> Go (sSimplify s1)
        _ -> s
   in if s' == s
        then s'
        else sSimplify s'

-- | Simplify IR expressions.
eSimplify :: 𝐸 -> 𝐸
eSimplify pe =
  let bin c e1 e2 = c (eSimplify e1) (eSimplify e2)
      e' = case pe of
        -- n1 + n2 ==> n
        Const n1 :+ Const n2 -> ((n1 + n2) #)
        -- e + 0 ==> e
        e :+ Const 0 -> eSimplify e
        -- 0 + e ==> e
        Const 0 :+ e -> eSimplify e
        e1 :+ e2 -> bin (:+) e1 e2
        -- n1 - n2 ==> n
        Const n1 :- Const n2 -> ((n1 - n2) #)
        -- e - 0 ==> e
        e :- Const 0 -> eSimplify e
        -- e - e ==> 0
        e1 :- e2 -> if e1 == e2 then (0 #) else bin (:-) e1 e2
        -- n1 * n2 ==> n
        Const n1 :* Const n2 -> ((n1 * n2) #)
        -- 1 * e ==> e
        Const 1 :* e -> eSimplify e
        -- e * 1 ==> e
        e :* Const 1 -> eSimplify e
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
        else eSimplify e'
