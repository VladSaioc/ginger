module Go.Utilities where

import Go.Ast
import Utilities.Position

-- Turns:
-- if e1 s1
-- else if e2 s2
-- ...
-- else if en sn
-- else { ... }
--
-- Into:
-- if en sn
-- ...
-- else if e2 s2
-- else if e1 s1
-- else { ... }
flipIfs :: Pos Stmt -> Pos Stmt
flipIfs s =
  let collectBranches = \case
        Pos p (If e s' [els]) -> Pos p (e, s') : collectBranches els
        _ -> []
      getElse = \case
        Pos _ (If _ _ [els]) -> getElse els
        Pos p (If _ _ els) -> Pos p $ Block els
        ps' -> ps'
      branches = collectBranches s
   in foldl (\(Pos p els) (Pos p' (e, s')) -> Pos p' $ If e s' [Pos p els]) (getElse s) branches