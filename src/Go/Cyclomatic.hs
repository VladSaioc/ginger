module Go.Cyclomatic where

import Go.Ast
import Utilities.Position

-- simpleProcess checks whether a sequence of statements constitutes
-- a *simple* process.
--
-- A process is deemed simple if:
-- 1. It is linear
-- 2. It has no decision, TODO: or topologically equivalent communication branches
-- 3. Does not spawn additional processes
-- 4. Does not create additional channels
simpleProcess :: [Pos Stmt] -> Maybe ()
simpleProcess = \case
  [] -> return ()
  Pos _ s : ss -> do
    _ <- case s of
      Skip -> return ()
      Return -> return ()
      As {} -> return ()
      Chan {} -> Nothing
      Break -> Nothing
      Atomic _ -> return ()
      Close {} -> return ()
      Decl {} -> return ()
      Block ss' -> simpleProcess ss'
      -- TODO: Compare branch topology here OR write some simplification
      If {} -> Nothing
      -- TODO: Compare branch topology here OR write some simplification
      Select {} -> Nothing
      For {} -> Nothing
      Go {} -> Nothing
      While {} -> Nothing
    simpleProcess ss
