module Go.StreamlineReturn where

import Go.Ast
import Utilities.Position

streamlineReturn :: Prog -> Prog
streamlineReturn (Prog ss) = Prog $ streamlineReturnSs $ ss ++ [Pos NoPos Return]

-- Inductive
streamlineReturnSs :: [Pos Stmt] -> [Pos Stmt]
streamlineReturnSs = \case
  [] -> []
  Pos p s : ss ->
    let pos = Pos p
        un c s1 = pos $ c (streamlineReturnSs s1)
     in case s of
          Skip -> streamlineReturnSs ss
          Return -> [pos s]
          Chan {} -> pos s : streamlineReturnSs ss
          Break -> [pos s]
          Atomic {} -> pos s : streamlineReturnSs ss
          Decl {} -> pos s : streamlineReturnSs ss
          As {} -> pos s : streamlineReturnSs ss
          Close {} -> pos s : streamlineReturnSs ss
          Block ss' ->
            streamlineReturnSs ss'
              ++ if terminal ss'
                then []
                else streamlineReturnSs ss
          If g ss1 ss2 ->
            let ss1' = streamlineReturnSs ss1
                ss2' = streamlineReturnSs ss2
                ss' = streamlineReturnSs ss
             in if terminal ss1
                  then [pos $ If g ss1' (ss2' ++ ss')]
                  else
                    if terminal ss2
                      then [pos $ If g (ss1' ++ ss') ss2']
                      else pos (If g ss1' ss2') : ss'
          For x e1 e2 d ss1 ->
            let ss1' = streamlineReturnSs ss1
                ss' = streamlineReturnSs ss
             in if terminal ss1'
                  then pos (Decl x e1) : ss1'
                  else pos (For x e1 e2 d ss1') : ss'
          Select cs d ->
            let d' = fmap streamlineReturnSs d
                cs' = zip (map fst cs) $ map (streamlineReturnSs . snd) cs
                ss' = streamlineReturnSs ss
             in if terminal [pos $ Select cs' d']
                  then [pos $ Select cs' d']
                  else pos (Select cs' d') : ss'
          While e ss' ->
            let ss'' = streamlineReturnSs ss'
             in if terminal ss''
                  then ss''
                  else pos (While e ss'') : streamlineReturnSs ss
          Go ss' -> un Go ss' : streamlineReturnSs ss

-- Determines whether a sequence of Go statements is 'terminal'
-- i.e., every one of its execution paths terminates with a 'return'
-- statement.
--
-- It is useful in stream-lining if statements.
-- Let <_> be a short-hand for the 'terminal' predicate. The rules are:
--
--  [RETURN]:   <return : ss>
--  [SKIP]:     <skip : ss>
--              |- <ss>
--  [DECL]:     <x := e : ss>
--              |- <ss>
--  [ASSIGN]:   <x = e : ss>
--              |- <ss>
--  [CLOSE]:    <close(c) : ss>
--              |- <ss>
--  [BLOCK-1]:  <{ ss' } : ss>
--              |- <ss'>
--  [BLOCK-2]:  <{ ss' } : ss>
--              |- <ss>
--  [IF-1]:     <if e then ss1 else ss2 : ss>
--              |- <ss1>
--              |- <ss2>
--  [IF-2]:     <if e then ss1 else ss2 : ss>
--              |- <ss>
--  [SELECT-1]: <select { case o: ss₁ | ... | case o: ssₙ | default: ss₀ } : ss>
--              |- ∀ 0 ≤ i ≤ n. <ssᵢ>
--  [SELECT-2]: <select { case o: ss₁ | ... | case o: ssₙ } : ss>
--              |- ∀ 1 ≤ i ≤ n. <ssᵢ>
--  [SELECT-3]: <select { _ } : ss>
--              |- <ss>
--  [FOR-1]:    <for _ { ss' } : ss>
--              |- <ss'>
--  [FOR-2]:    <for _ { ss' } : ss>
--              |- <ss>
--  [GO]:       <go { ss' } : ss>
--              |- <ss>
terminal :: [Pos Stmt] -> Bool
terminal = \case
  [] -> False
  Pos _ s : ss -> case s of
    Return -> True
    Skip -> terminal ss
    Break {} -> terminal ss
    Chan {} -> terminal ss
    Decl {} -> terminal ss
    As {} -> terminal ss
    Close {} -> terminal ss
    Atomic {} -> terminal ss
    Block ss' -> terminal ss' || terminal ss
    If _ ss1 ss2 -> (terminal ss1 && terminal ss2) || terminal ss
    Select cs mds ->
      let tmds = maybe True terminal mds
          tcs = all (terminal . snd) cs
       in (tcs && tmds) || terminal ss
    For _ _ _ _ ss' -> terminal ss' || terminal ss
    While _ ss' -> terminal ss' || terminal ss
    Go _ -> terminal ss
