module IR.GetAst (getAst) where

import IR.AbsIR qualified as R'
import IR.Ast
import IR.ErrM qualified as R''
import IR.ParIR
import Utilities.Err as U
import Utilities.General

(#) :: R'.NUMBER -> Int
(#) (R'.NUMBER (_, i)) = read i :: Int

(&) :: R'.ID -> String
(&) (R'.ID (_, x)) = x

foldM :: Monad m => (a -> m b) -> [a] -> m [b]
foldM f rs =
  let (|>) r1 r2 = do
        o1 <- r1
        o2 <- f r2
        return (o2 : o1)
   in do
        os' <- foldl (|>) (return []) rs
        return (reverse os')

-- Parses the given string as a Promela program and
-- performs additional refinement on the existing parse tree.
getAst :: String -> Err Prog
getAst = pProgram . pProg . myLexer

pProgram :: R''.Err R'.Prog -> Err Prog
pProgram = \case
  R''.Ok (R'.Prog chs ps) -> do
    chs' <- foldM pChan chs
    ps' <- foldM pProc ps
    return (Prog chs' ps')
  R''.Bad err -> Bad err

pChan :: R'.Chan -> Err Chan
pChan (R'.Chan c e) = do
  e' <- pExp e
  return (Chan (c &) e')

pProc :: R'.Proc -> Err Stmt
pProc (R'.Proc _ ss) =
  let (|>) rs1 rs2 = do
        s1 <- rs1
        s2 <- pStm rs2
        return (Seq s1 s2)
   in foldl (|>) (Ok Skip) ss

pStm :: R'.Stm -> Err Stmt
pStm = \case
  R'.SOp o -> do
    o' <- pOp o
    return (Atomic o')
  R'.For _ x e1 e2 os -> do
    e1' <- pExp e1
    e2' <- pExp e2
    os' <- foldM pOp os
    return (For (x &) e1' e2' os')

pOp :: R'.Op -> Err Op
pOp = \case
  R'.Snd c _ -> return (Send (c &))
  R'.Rcv c _ -> return (Recv (c &))

pExp :: R'.Exp -> Err Exp
pExp =
  let bin = binaryCons pExp
   in \case
        R'.Plus e1 _ e2 -> bin Plus e1 e2
        R'.Minus e1 _ e2 -> bin Minus e1 e2
        R'.Mult e1 _ e2 -> bin Mult e1 e2
        R'.Div {} -> Bad "Unsupported division"
        R'.Const n -> return (Const (n #))
        R'.Var x -> return (Var (x &))