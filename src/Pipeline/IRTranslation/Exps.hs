module Pipeline.IRTranslation.Exps (parseExp) where

import Backend.Ast qualified as T
import IR.Ast

parseExp :: Exp -> T.Exp
parseExp =
  let bin c e1 e2 = c (parseExp e1) (parseExp e2)
   in \case
        Plus e1 e2 -> bin T.Plus e1 e2
        Minus e1 e2 -> bin T.Minus e1 e2
        Div e1 e2 -> bin T.Div e1 e2
        Mult e1 e2 -> bin T.Mult e1 e2
        Const n -> T.ECon (T.CNum n)
        Var x -> T.EVar x
