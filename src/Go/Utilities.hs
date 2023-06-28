module Go.Utilities where

import Go.Ast

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
flipIfs :: Stmt -> Stmt
flipIfs s =
  let collectBranches = \case
        If e s' [els] -> (e, s') : collectBranches els
        _ -> []
      getElse = \case
        If _ _ [els] -> getElse els
        If _ _ els -> Block els
        s' -> s'
      branches = collectBranches s
   in foldl (\els (e, s') -> If e s' [els]) (getElse s) branches