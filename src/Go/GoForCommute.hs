module Go.GoForCommute where

import Go.Ast
import Go.Cyclomatic
import Utilities.Position

data Goroutine = Goroutine
  { decls :: [Pos Stmt],
    body :: [Pos Stmt],
    pos :: Int
  }
  deriving (Eq, Ord, Read, Show)

-- goForCommute attempts to commute appropriate 'go' and 'for' statements.
-- A 'go' and 'for' statements may be commuted iff. the communication pattern
-- of the goroutines spawned in the body of the 'for' exhibit simple behaviour,
-- and if there are no side effects outside the
--
--
-- Example:
-- for (x : e1 .. e2) {
--    go { S1 }; go { S2 }; ...; go { Sn }
-- }
--
-- becomes
--
-- go {
--    for (x : e1 .. e2) { S1 }
-- }
-- go {
--    for (x : e1 .. e2) { S2 }
-- }
-- ...
-- go {
--    for (x : e1 .. e2) { Sn }
-- }
goForCommute :: Prog -> Prog
goForCommute (Prog ss) = Prog (goForCommuteSpine ss)

goForCommuteSpine :: [Pos Stmt] -> [Pos Stmt]
goForCommuteSpine = \case
  [] -> []
  Pos p s : ss ->
    let un c = c . goForCommuteSpine
        bin c s1 s2 = c (goForCommuteSpine s1) (goForCommuteSpine s2)
        s'' = case s of
          Block s' -> un Block s'
          If e s1 s2 -> bin (If e) s1 s2
          While e s' -> un (While e) s'
          Go s' -> un Go s'
          For x e1 e2 d s' ->
            case processForBody s' of
              Nothing -> un (For x e1 e2 d) s'
              Just gs' ->
                let makeGo (Goroutine {decls, body, pos}) =
                      let g = Go $ decls ++ [Pos p $ For x e1 e2 d body]
                       in Pos pos g
                 in Block $ map makeGo gs'
          s' -> s'
     in Pos p s'' : goForCommuteSpine ss

-- processForBody inspects the body of a for loop and checks whether its body
-- consists only of simple goroutine spawns.
-- If it is the case, it returns just the bodies of the goroutines.
-- Otherwise, it returns nothing.
processForBody :: [Pos Stmt] -> Maybe [Goroutine]
processForBody = \case
  [] -> Just []
  Pos p s : ss -> case s of
    Skip -> processForBody ss
    Break -> Nothing
    Return -> Nothing
    Decl {} -> Nothing
    As {} -> Nothing
    Chan {} -> Nothing
    Atomic {} -> Nothing
    Close {} -> Nothing
    Block ss' -> processForBody $ ss' ++ ss
    If {} -> Nothing
    Select {} -> Nothing
    For {} -> Nothing
    While {} -> Nothing
    Go s' -> do
      _ <- simpleProcess s'
      ds <- goroutineDecls s'
      let stmts = goroutineStmts s'
      let g = Goroutine {decls = ds, body = stmts, pos = p}
      ss' <- processForBody ss
      return $ g : ss'

-- goroutineDecls collects all the upfront declarations in a goroutine's body.
goroutineDecls :: [Pos Stmt] -> Maybe [Pos Stmt]
goroutineDecls = \case
  [] -> return []
  Pos p s : ss -> case s of
    Decl {} -> do
      ss' <- goroutineDecls ss
      return $ Pos p s : ss'
    Block ss' -> goroutineDecls $ ss' ++ ss
    _ -> return []

-- goroutineStmts collects all the non-declaration statements in a goroutine's body.
goroutineStmts :: [Pos Stmt] -> [Pos Stmt]
goroutineStmts ss =
  let addStmt = \case
        Pos _ Decl {} -> id
        s -> (s :)
   in Prelude.foldr addStmt [] ss
