module Trace.GetAst where

import Control.Monad
import Data.Map qualified as Map
import Data.Set qualified as Set
import Trace.AbsTrace qualified as Raw
import Trace.Ast
import Trace.ErrM qualified as RawErr
import Trace.ParTrace qualified as Parser
import Utilities.Err
import Utilities.General
import Utilities.Position

initTrace =
  Trace
    { steps = [],
      notTerminated = Set.empty,
      processes = Map.empty
    }

-- Parses the given string as a Trace and performs
-- additional refinement on the existing parse tree.
getAst :: String -> Err (Trace Pos)
getAst = pTrace . Parser.pTrace . Parser.myLexer

pTrace :: RawErr.Err Raw.Trace -> Err (Trace Pos)
pTrace = \case
  RawErr.Ok (Raw.Trace ss fs _) -> Bad "Not implemented" -- pSteps initTrace ss
  RawErr.Bad msg -> Bad msg

-- pPos :: Monad m => Raw.Path -> m Int
-- pPos (Raw.Path _ n) = return (fromIntegral n :: Int)

-- pPid :: Monad m => Raw.Pref -> m Int
-- pPid (Raw.Pref _ pid _) = return pid

-- pSteps :: Trace Pos -> [Raw.Step] -> Err (Trace Pos)
-- pSteps trace = \case
--   [] -> return (trace { steps = reverse (steps trace) })
--   (Raw.Start (Raw.Ident f) cid) :
--     (Raw.Stmt pid pos _ (Raw.Expr (Raw.Run (Raw.Ident g) es))) :
--     ss ->
--       if g /= f then
--         Bad ("Start and 'run' statements are in disagreement about the procedure name." ++
--           "Started procedure is " ++ show f ++
--           " but 'run' instruction invokes " ++ show g)
--       else do
--         pos <- pPos pos
--         pid <- pPid pid
--         es <- results (Prelude.map pExp es)
--         -- (fun, pss) <- ()
--         trace <- return (
--           let ss = steps trace
--           in let ps = processes trace
--           in let Just (fun, pss) = Map.lookup pid ps
--           in let step = Pos pos (Start cid f es)
--           in let
--             updatedProcesses = ps
--               $ (Map.insert cid (f, []))
--               $ (Map.insert pid (fun, step : pss))
--           in trace {
--             steps = step : ss,
--             processes = updatedProcesses
--             }
--           )
--         pSteps trace ss
--   (Raw.Start f pid) : ss ->
--     Bad ("Trace is not well-formed: " ++
--       "Thread " ++ show pid ++ " was started," ++
--       "but the subsequent instruction was not a 'run' step")

-- pExp :: Raw.Exp -> Err Exp
-- pExp =
--   let bin = binaryCons pExp
--   in let unExp = unaryCons pExp
--   in let unVar = unaryCons pLVal
--   in \case
--     Raw.Or e1 e2 -> bin Or e1 e2
--     Raw.And e1 e2 -> bin And e1 e2
--     Raw.Eq e1 e2 -> bin Eq e1 e2
--     Raw.Ne e1 e2 -> bin Ne e1 e2
--     Raw.Le e1 e2 -> bin Le e1 e2
--     Raw.Ge e1 e2 -> bin Ge e1 e2
--     Raw.Lt e1 e2 -> bin Lt e1 e2
--     Raw.Gt e1 e2 -> bin Gt e1 e2
--     Raw.Plus e1 e2 -> bin Plus e1 e2
--     Raw.Minus e1 e2 -> bin Minus e1 e2
--     Raw.Prod e1 e2 -> bin Prod e1 e2
--     Raw.Div e1 e2 -> bin Div e1 e2
--     Raw.Neg e -> unExp Neg e
--     Raw.Not e -> unExp Not e
--     Raw.Len v -> unVar Len v
--     Raw.Var v -> unVar Var v
--     Raw.Const n -> return (Const n)
--     Raw.Run (Raw.Ident f) _ ->
--       Bad ("Run statement for " ++ f ++ " encountered in isolation.")

-- pLVal :: Raw.LVal -> Err LVal
-- pLVal = \case
--   Raw.Id (Raw.Ident x) -> return (Id x)
--   Raw.Arr v e -> do
--     v <- pLVal v
--     e <- pExp e
--     return (Arr v e)
--   Raw.Field v (Raw.Ident f) -> do
--     v <- pLVal v
--     return (Field v f)
