module IR.Homogeneity (homogeneous) where

import Data.Map qualified as M
import IR.Ast
import IR.Utilities
import Utilities.Err
import Utilities.General

type DirEnv = Err (M.Map String OpDir)

homogeneous :: Prog -> Err ()
homogeneous (Prog _ gos) = do
  _ <- results (Prelude.map homogeneousProc gos)
  return ()

homogeneousProc :: Stmt -> Err ()
homogeneousProc s = do
  _ <- homogeneousStmt (return M.empty) s
  return ()

homogeneousStmt :: DirEnv -> Stmt -> DirEnv
homogeneousStmt d = \case
  Seq s1 s2 -> do
    d' <- homogeneousStmt d s1
    homogeneousStmt (Ok d') s2
  Atomic o -> homogeneousOp d o
  For _ _ _ os -> foldl homogeneousOp d os
  Skip -> d

homogeneousOp :: DirEnv -> Op -> DirEnv
homogeneousOp menv op =
  let (c, d) = (chName op, chDir op)
   in do
        env <- menv
        case M.lookup c env of
          Just op ->
            if op == d
              then return env
              else Bad ("Operations of channel " ++ c ++ " are not homogeneous.")
          Nothing -> return (M.insert c d env)
