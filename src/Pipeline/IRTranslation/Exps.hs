module Pipeline.IRTranslation.Exps (parseExp) where

import Backend.Ast qualified as T
import Backend.Utilities
import IR.Ast

parseExp :: Exp -> T.Exp
parseExp =
  let bin c e1 e2 = c (parseExp e1) (parseExp e2)
   in \case
        BTrue -> (True ?)
        BFalse -> (False ?)
        And e1 e2 -> bin (T.:&&) e1 e2
        Or e1 e2 -> bin (T.:||) e1 e2
        Not e -> T.Not (parseExp e)
        Eq e1 e2 -> bin (T.:==) e1 e2
        Ne e1 e2 -> bin (T.:!=) e1 e2
        Lt e1 e2 -> bin (T.:<) e1 e2
        Leq e1 e2 -> bin (T.:<=) e1 e2
        Gt e1 e2 -> bin (T.:>) e1 e2
        Geq e1 e2 -> bin (T.:>=) e1 e2
        Plus e1 e2 -> bin (T.:+) e1 e2
        Minus e1 e2 -> bin (T.:-) e1 e2
        Div e1 e2 -> bin (T.:/) e1 e2
        Mult e1 e2 -> bin (T.:*) e1 e2
        Const n -> T.ECon (T.CNum n)
        Var x -> T.EVar x
