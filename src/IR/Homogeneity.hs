module IR.Homogeneity (homogeneous) where

import Data.Map qualified as M
import IR.Ast
import IR.Utilities
import Utilities.Err

homogeneous = 0

-- type DirEnv = Err (M.Map String OpType)

-- homogeneous :: 𝑃 -> Err ()
-- homogeneous (𝑃 _ gos) = do
--   _ <- homogeneousProc gos
--   return ()

-- homogeneousProc :: 𝑆 -> Err ()
-- homogeneousProc s = do
--   _ <- homogeneousStmt (return M.empty) s
--   return ()

-- homogeneousStmt :: DirEnv -> 𝑆 -> DirEnv
-- homogeneousStmt d = \case
--   Return -> d
--   Go s -> homogeneousStmt (return M.empty) s
--   If _ s1 s2 -> do
--     d' <- homogeneousStmt d s1
--     homogeneousStmt (return d') s2
--   Seq s1 s2 -> do
--     d' <- homogeneousStmt d s1
--     homogeneousStmt (return d') s2
--   Atomic o -> homogeneousOp d o
--   For _ _ _ os -> foldl homogeneousOp d os
--   Skip -> d

-- homogeneousOp :: DirEnv -> Op -> DirEnv
-- homogeneousOp menv o =
--   let (c, d) = (primName o, opType o)
--    in do
--         env <- menv
--         case M.lookup c env of
--           Just o' ->
--             if o' == d
--               then return env
--               else Bad ("Operations of channel " ++ c ++ " are not homogeneous.")
--           Nothing -> return (M.insert c d env)
