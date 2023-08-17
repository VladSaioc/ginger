module Go.StreamlineReturn where

import Go.Ast
import Go.Cyclomatic
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
