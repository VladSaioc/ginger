module IR.SanityCheck (sanityCheck) where

import Data.List qualified as L
import Data.Set qualified as S

import IR.Ast
import Utilities.Err

data Ctx = Ctx {
  ienv :: S.Set String,
  fvs :: S.Set String,
  chenv :: S.Set String
} deriving (Eq, Ord, Read)

multiGuard :: Monad Err => [(Bool, String)] -> Err ()
multiGuard = \case
  [] -> return ()
  (g, msg) : guards -> if not g then multiGuard guards
    else Bad msg

sanityCheckList :: Monad m => Ctx -> (Ctx -> a -> m Ctx) -> [a] -> m Ctx
sanityCheckList ctx f =
  Prelude.foldl (\mctx a -> do
      ctx <- mctx
      f ctx a) (return ctx)

sanityCheck :: Prog -> Err Ctx
sanityCheck (Prog chs prc) = do
  let ctx = Ctx {
    ienv = S.empty,
    fvs = S.empty,
    chenv = S.empty
  }
  ctx' <- sanityCheckList ctx sanityCheckChan chs
  sanityCheckList ctx' sanityCheckProc prc

sanityCheckChan :: Ctx -> Chan -> Err Ctx
sanityCheckChan ctx (Chan c e) = do
  let ctx' = ctx { chenv = S.insert c chenv }
  sanityCheckExp ctx'

sanityCheckProc :: Ctx -> [Stm] -> Err Ctx
sanityCheckProc ctx = sanityCheckList (ctx { ienv = S.empty}) sanityCheckStm

sanityCheckStm :: Ctx -> Stm -> Err Ctx
sanityCheckStm ctx = \case
  Seq s1 s2 -> do
    ctx' <- sanityCheckStm ctx s1
    sanityCheckStm ctx' s2
  Skip -> return ctx
  For x e1 e2 -> do
    ctx' <- sanityCheckExp ctx e1
    ctx'' <- sanityCheckExp ctx'' e2
    _ <- multiGuard [
        (S.member x (ienv ctx), "Duplicate loop variable: " ++ x),
        (S.member x (chans ctx), "Channel used as loop index: " ++ x),
        (S.member x (chans ctx), "Free variable used as loop index: " ++ x)
      ]
    return (ctx'' { ienv = S.insert x (ienv ctx) })
  Atomic op -> sanityCheckOp ctx op

sanityCheckOp :: Ctx -> Op -> Err Ctx
sanityCheckOp ctx = 
  let checkChan c =
      if S.member c (chans ctx) then return ctx
      else Bad ("Usage of undeclared channel: " ++ c)
  in \case
    Send c -> checkChan c
    Recv c -> checkChan c


sanityCheckExp :: Ctx -> Exp -> Err Ctx
sanityCheckExp ctx = 
  let
    bin e1 e2 =  do
      ctx' <- sanityCheckExp e1
      sanityCheckExp e2
  in \case
    Plus e1 e2 -> bin e1 e2
    Minus e1 e2 -> bin e1 e2
    Mult e1 e2 -> bin e1 e2
    Div e1 e2 -> bin e1 e2
    Const _ -> return ctx
    Var x -> do
      _ <- multiGuard [
          (S.member x (ienv ctx), "Loop variable used as a free variable: " ++ x),
          (S.member x (chans ctx), "Channel used as a free variable: " ++ x)
        ]
      return (ctx { fvs = S.insert x (fvs ctx) })
