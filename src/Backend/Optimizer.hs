module Backend.Optimizer where

import Backend.Ast
import Backend.Utilities

optimize :: Program -> Program
optimize (Program ds) = Program $ map decOptimize ds

decOptimize :: Decl -> Decl
decOptimize = \case
  FDecl f -> FDecl $ fOptimize f
  MDecl m -> MDecl $ mOptimize m
  LDecl m -> LDecl $ mOptimize m
  d -> d

fOptimize :: Function -> Function
fOptimize (Function {ghost, yields, funcHoare, funcBody}) =
  Function
    { ghost = ghost,
      yields = yields,
      funcHoare = hOptimize funcHoare,
      funcBody = eOptimize funcBody
    }

mOptimize :: Method -> Method
mOptimize (Method {returns, methodHoare, methodBody}) =
  Method
    { returns = returns,
      methodHoare = hOptimize methodHoare,
      methodBody = sOptimize methodBody
    }

hOptimize :: HoareWrap -> HoareWrap
hOptimize (HoareWrap {name, params, decreases, requires, ensures}) =
  HoareWrap
    { name = name,
      params = params,
      decreases = map eOptimize decreases,
      requires = map eOptimize requires,
      ensures = map eOptimize ensures
    }

eOptimize :: Exp -> Exp
eOptimize pe =
  let bin c e1 e2 = c (eOptimize e1) (eOptimize e2)
      un c e = c (eOptimize e)
      tri c e1 e2 e3 = c (eOptimize e1) (eOptimize e2) (eOptimize e3)
      e' = case pe of
        Exists xs e -> un (Exists xs) e
        Forall xs e -> un (Forall xs) e
        -- false ==> e ==> true
        Implies (ECon CFalse) _ -> ECon CTrue
        -- e ==> true ==> true
        Implies _ (ECon CTrue) -> ECon CTrue
        Implies e1 e2 -> bin Implies e1 e2
        -- true <==> false ==> false
        Equiv (ECon CTrue) (ECon CFalse) -> ECon CFalse
        -- false <==> true ==> false
        Equiv (ECon CFalse) (ECon CTrue) -> ECon CFalse
        -- e <==> e ==> true
        Equiv e1 e2 ->
          if e1 == e2
            then ECon CTrue
            else bin Equiv e1 e2
        -- e && false ==> false
        And _ (ECon CFalse) -> ECon CFalse
        -- false && e ==> false
        And (ECon CFalse) _ -> ECon CFalse
        -- e && true ==> e
        And e (ECon CTrue) -> eOptimize e
        -- true && e ==> e
        And (ECon CTrue) e -> eOptimize e
        And e1 e2 -> bin And e1 e2
        -- e || true ==> true
        Or _ (ECon CTrue) -> ECon CTrue
        -- true || e ==> true
        Or (ECon CTrue) _ -> ECon CTrue
        -- e || false ==> e
        Or e (ECon CFalse) -> eOptimize e
        -- false || e ==> e
        Or (ECon CFalse) e -> eOptimize e
        Or e1 e2 -> bin Or e1 e2
        -- !true ==> false
        Not (ECon CTrue) -> ECon CFalse
        -- !false ==> true
        Not (ECon CFalse) -> ECon CTrue
        Not e -> un Not e
        -- e == e ==> true
        Eq e1 e2 ->
          if e1 == e2
            then ECon CTrue
            else bin Eq e1 e2
        -- e != e ==> false
        Ne e1 e2 ->
          if e1 == e2
            then ECon CFalse
            else bin Ne e1 e2
        -- n1 <= n2
        Leq (ECon (CNum n1)) (ECon (CNum n2)) -> ECon $ if n1 <= n2 then CTrue else CFalse
        -- e <= e ==> true
        Leq e1 e2 ->
          if e1 == e2
            then ECon CTrue
            else bin Leq e1 e2
        -- n1 < n2
        Lt (ECon (CNum n1)) (ECon (CNum n2)) -> ECon $ if n1 < n2 then CTrue else CFalse
        -- e < e ==> false
        Lt e1 e2 ->
          if e1 == e2
            then ECon CFalse
            else bin Lt e1 e2
        -- n1 >= n2
        Geq (ECon (CNum n1)) (ECon (CNum n2)) -> ECon $ if n1 >= n2 then CTrue else CFalse
        -- e >= e ==> true
        Geq e1 e2 ->
          if e1 == e2
            then ECon CTrue
            else bin Geq e1 e2
        -- n1 > n2
        Gt (ECon (CNum n1)) (ECon (CNum n2)) -> ECon $ if n1 > n2 then CTrue else CFalse
        -- e > e ==> false
        Gt e1 e2 ->
          if e1 == e2
            then ECon CFalse
            else bin Gt e1 e2
        -- n1 + n2 ==> n
        Plus (ECon (CNum n1)) (ECon (CNum n2)) -> ((n1 + n2) #)
        -- e + 0 ==> e
        Plus e (ECon (CNum 0)) -> eOptimize e
        -- 0 + e ==> e
        Plus (ECon (CNum 0)) e -> eOptimize e
        Plus e1 e2 -> bin Plus e1 e2
        -- n1 - n2 ==> n
        Minus (ECon (CNum n1)) (ECon (CNum n2)) -> ((n1 - n2) #)
        -- e - 0 ==> e
        Minus e (ECon (CNum 0)) -> eOptimize e
        Minus e1 e2 -> bin Minus e1 e2
        -- n1 * n2 ==> n
        Mult (ECon (CNum n1)) (ECon (CNum n2)) -> ((n1 * n2) #)
        -- 1 * e ==> e
        Mult (ECon (CNum 1)) e -> eOptimize e
        -- e * 1 ==> e
        Mult e (ECon (CNum 1)) -> eOptimize e
        -- e * 0 ==> 0
        Mult _ (ECon (CNum 0)) -> (0 #)
        -- 0 * e ==> e
        Mult (ECon (CNum 0)) _ -> (0 #)
        Mult e1 e2 -> bin Mult e1 e2
        -- e / 1 ==> e
        Div e (ECon (CNum 1)) -> e
        Div e1 e2 -> bin Div e1 e2
        -- if true then e1 else e2 ==> e1
        IfElse (ECon CTrue) e _ -> eOptimize e
        -- if true then e1 else e2 ==> e1
        IfElse (ECon CFalse) _ e -> eOptimize e
        IfElse e1 e2 e3 -> tri IfElse e1 e2 e3
        Match e cs ->
          let ps = map fst cs
              es = map (eOptimize . snd) cs
           in Match (eOptimize e) $ zip ps es
        Call f es -> Call f $ map eOptimize es
        Mod e1 e2 -> bin Mod e1 e2
        e -> e
   in if pe == e'
        then e'
        else eOptimize e'

sOptimize :: Stmt -> Stmt
sOptimize = \case
  Assign ds ->
    let xs = map fst ds
        es = map (eOptimize . snd) ds
     in Assign $ zip xs es
  Block ss -> Block $ map sOptimize ss
  Assert e -> Assert $ eOptimize e
  VarDef b ds ->
    let ds' = map (\(x, t, e) -> (x, t, eOptimize e)) ds
     in VarDef b ds'
  If e s1 ms ->
    let e' = eOptimize e
        s1' = sOptimize s1
        ms' = fmap sOptimize ms
     in case e of
          ECon CTrue -> s1'
          ECon CFalse ->
            ( case ms' of
                Just s2 -> s2
                Nothing -> Block []
            )
          _ -> If e' s1' ms'
  MatchStmt e cs ->
    let ps = map fst cs
        ss = map (sOptimize . snd) cs
     in MatchStmt (eOptimize e) $ zip ps ss
  While e es1 es2 s -> While (eOptimize e) (map eOptimize es1) (map eOptimize es2) (sOptimize s)
  Return es -> Return $ map eOptimize es
