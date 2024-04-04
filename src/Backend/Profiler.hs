module Backend.Profiler (sizeOfExpr) where

import Backend.Ast

sizeOfExpr :: Exp -> Int
sizeOfExpr =
  let un e = 1 + sizeOfExpr e
      bin e1 e2 = 1 + sizeOfExpr e1 + sizeOfExpr e2
   in \case
    IfElse e1 e2 e3 -> 1 + sizeOfExpr e1 + sizeOfExpr e2 + sizeOfExpr e3
    Match e ps -> 1 + sizeOfExpr e + sum (map (\(_, s) -> sizeOfExpr s) ps)
    ETuple es -> 1 + sum (map sizeOfExpr es)
    ESet es -> 1 + sum (map sizeOfExpr es)
    Exists _ e -> un e
    Forall _ e -> un e
    In e1 e2 -> bin e1 e2
    ECon _ -> 1
    EVar _ -> 1
    Any -> 0
    e1 :<==> e2 -> bin e1 e2
    e1 :==> e2 -> bin e1 e2
    e1 :&& e2 -> bin e1 e2
    e1 :|| e2 -> bin e1 e2
    e1 :== e2 -> bin e1 e2
    e1 :!= e2 -> bin e1 e2
    e1 :>= e2 -> bin e1 e2
    e1 :<= e2 -> bin e1 e2
    e1 :> e2 -> bin e1 e2
    e1 :< e2 -> bin e1 e2
    e1 :+ e2 -> bin e1 e2
    e1 :- e2 -> bin e1 e2
    e1 :* e2 -> bin e1 e2
    e1 :/ e2 -> bin e1 e2
    e1 :% e2 -> bin e1 e2
    Not e -> un e
    Neg e -> un e
    Call _ es -> 1 + sum (map sizeOfExpr es)
