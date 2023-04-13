module IR.Homogeneity (homogeneous) where

import Data.Map qualified as M
import IR.Ast

data Dir = S | R deriving (Eq)

type DirEnv = Maybe (M.Map String Dir)

homogeneous :: Prog -> Bool
homogeneous (Prog _ gos) =
  foldl homogeneousProc True gos

homogeneousProc :: Bool -> Stmt -> Bool
homogeneousProc ok s =
  ok && case homogeneousStmt (Just M.empty) s of
    Just _ -> True
    Nothing -> False

homogeneousStmt :: DirEnv -> Stmt -> DirEnv
homogeneousStmt d = \case
  Seq s1 s2 -> do
    d'' <- homogeneousStmt d s1
    homogeneousStmt (Just d'') s2
  Atomic o -> homogeneousOp d o
  For _ _ os -> foldl homogeneousOp d os

homogeneousOp :: DirEnv -> Op -> DirEnv
homogeneousOp d commOp =
  let (ch, dir) = case commOp of
        Send c -> (c, S)
        Recv c -> (c, R)
   in do
        env <- d
        case M.lookup ch env of
          Just op ->
            if op == dir
              then return env
              else Nothing
          Nothing -> return (M.insert ch dir env)
