module Go.ZipCases where

import Data.Bifunctor
import Debug.Trace (trace)
import Go.Ast
import Go.Cyclomatic
import Utilities.Position

{- Zip cases by computing
-}
zipCases :: Prog -> Prog
zipCases (Prog s) =
  let s' = zipCasesStmts $ s ++ [Pos NoPos Return]
   in Prog s'

zipCasesStmts :: [Pos Stmt] -> [Pos Stmt]
zipCasesStmts = \case
  [] -> []
  s0@(Pos p s) : ss ->
    let ss' = zipCasesStmts ss
        twoBranches ss10 ss20 =
          let (ss1, ss2) = trace (show (show ss10) ++ "\n" ++ show (show ss20)) (ss10, ss20)
              (ss1', ss2', cont) = case (terminal ss1, terminal ss2) of
                (True, True) -> (zipCasesStmts ss1, zipCasesStmts ss2, [])
                (True, False) -> (zipCasesStmts ss1, zipCasesStmts (ss2 ++ ss), [])
                (False, True) -> (zipCasesStmts (ss1 ++ ss), zipCasesStmts ss2, [])
                (False, False) -> (zipCasesStmts ss2, zipCasesStmts ss2, ss')
              hoist c1 c2 = case (c1, c2) of
                ([], _) -> (c1, c2, [])
                (_, []) -> (c1, c2, [])
                (Pos p1 s1 : thn, Pos _ s2 : els) ->
                  let (thn', els', shd) = hoist thn els
                   in if s1 == s2
                        then (thn', els', Pos p1 s1 : shd)
                        else (c1, c2, [])
           in let (thn, els, pref) = hoist ss1' ss2'
                  (thn', els') =
                    if null pref
                      then (thn, els)
                      else (stripReturns thn, stripReturns els)
               in (thn', els', stripReturns pref, cont)
     in case s of
          Skip -> ss'
          As {} -> s0 : ss'
          Chan {} -> s0 : ss'
          For x e1 e2 inc ss1 ->
            let s' = For x e1 e2 inc (zipCasesStmts ss1)
             in Pos p s' : ss'
          Decl {} -> s0 : ss'
          Atomic {} -> s0 : ss'
          Close {} -> s0 : ss'
          Break -> [s0]
          Return -> [s0]
          Block ss1 -> zipCasesStmts ss1 ++ ss'
          Go ss1 ->
            let s' = Go (zipCasesStmts ss1)
             in Pos p s' : ss'
          While e ss1 ->
            let s' = While e $ zipCasesStmts ss1
             in Pos p s' : ss'
          If e ss1 ss2 ->
            let (thn, els, pref, cont) = twoBranches ss1 ss2
             in pref ++ Pos p (If e thn els) : cont
          Select [cs1, cs2] Nothing ->
            let (o1, ss1) = cs1
                (o2, ss2) = cs2
                (thn, els, pref, cont) = twoBranches ss1 ss2
             in pref ++ Pos p (Select [(o1, thn), (o2, els)] Nothing) : cont
          Select cs def ->
            let cs' = map (second zipCasesStmts) cs
                def' = fmap zipCasesStmts def
             in Pos p (Select cs' def') : ss'

-- let caseBodies = map snd cs
--     hoistStmts = fmap def
--     checkTerminal () c =
--         if terminal c
--           then
--           else
--     (ss1', ss2', cont) = case (terminal ss1, terminal ss2) of
--       (True, True) -> (zipCasesStmts ss1, zipCasesStmts ss2, [])
--       (True, False) -> (zipCasesStmts ss1, zipCasesStmts (ss2 ++ ss), [])
--       (False, True) -> (zipCasesStmts (ss1 ++ ss), zipCasesStmts ss2, [])
--       (False, False) -> (zipCasesStmts ss2, zipCasesStmts ss2, ss')
--     hoist c1 c2 = case (c1, c2) of
--       ([], _) -> (c1, c2, [])
--       (_, []) -> (c1, c2, [])
--       (Pos p1 s1 : thn, Pos _ s2 : els) ->
--         let (thn', els', shd) = hoist thn els
--          in if s1 == s2
--               then (thn', els', Pos p1 s1 : shd)
--               else (c1, c2, [])
--  in let (thn, els, pref) = hoist ss1' ss2'
--      in pref ++ Pos p (If e thn els) : cont