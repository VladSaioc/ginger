module Pipeline.IRTranslation.FreeVars (fvs) where

import Data.Set qualified as S
import IR.Ast

fvs :: Prog -> S.Set String
fvs (Prog chs procs) = S.unions (map chanFVs chs ++ map stmtFVs procs)

chanFVs :: Chan -> S.Set String
chanFVs (Chan _ e) = expFVs e

stmtFVs :: Stmt -> S.Set String
stmtFVs = \case
  Skip -> S.empty
  Atomic _ -> S.empty
  Seq s1 s2 -> S.union (stmtFVs s1) (stmtFVs s2)
  For _ e1 e2 _ -> S.union (expFVs e1) (expFVs e2)

expFVs :: Exp -> S.Set String
expFVs = \case
  Plus e1 e2 -> S.union (expFVs e1) (expFVs e2)
  Minus e1 e2 -> S.union (expFVs e1) (expFVs e2)
  Mult e1 e2 -> S.union (expFVs e1) (expFVs e2)
  Div e1 e2 -> S.union (expFVs e1) (expFVs e2)
  Const _ -> S.empty
  Var x -> S.singleton x