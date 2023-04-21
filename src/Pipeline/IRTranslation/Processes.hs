module Pipeline.IRTranslation.Processes (getProcs) where

import Backend.Ast qualified as T
import Data.Map qualified as M
import Data.Maybe qualified
import IR.Ast
import IR.Utilities
import Pipeline.IRTranslation.Exps
import Pipeline.IRTranslation.Utilities

-- Configurations include a binding for program points achieved so far,
-- and the next available ID for a fresh instruction
type Config = (Int, ProgPoints)

getProcs :: KEnv -> Prog -> Procs
getProcs kenv (Prog _ prcs) =
  let pidsAndSyntax = zip (take (length prcs) [0 ..]) prcs
      makeProc (pid, stmt) =
        let (n, pp) = stmtToPoints kenv pid (0, M.empty) stmt
            pp' = M.insert n (T.Block []) pp
         in (pid, pp')
   in M.fromList (map makeProc pidsAndSyntax)

stmtToPoints :: KEnv -> Pid -> Config -> Stmt -> Config
stmtToPoints kenv pid (n, pp) = \case
  Skip -> (n, pp)
  Seq s1 s2 ->
    let (n', pp') = stmtToPoints kenv pid (n, pp) s1
     in stmtToPoints kenv pid (n', pp') s2
  For x _ e ops ->
    let x' = pid % x
        e' = parseExp e
        (n', pp1) = opsToPoints kenv pid (n + 1, pp) ops

        -- x < e
        guard = T.Lt (T.EVar x') e'
        -- { pc := n + 1 }
        stay =
          T.Block
            [ T.Assign [((pid <|), T.ECon (T.CNum (n + 1)))]
            ]
        -- { pc := n' + 1 }
        leave =
          T.Block
            [ T.Assign [((pid <|), T.ECon (T.CNum (n' + 1)))]
            ]

        -- { x := x + 1; pc := n }
        iter =
          T.Block
            [ T.Assign [(x', T.Plus (T.EVar x') (T.ECon (T.CNum 1)))],
              T.Assign [((pid <|), T.ECon (T.CNum n))]
            ]

        -- n -> if x < e { pc := n + 1; } else { pc := n' + 1 }
        pp2 = M.insert n (T.If guard stay (Just leave)) pp1
        -- n' -> { x := x + 1; pc := n }
        pp3 = M.insert n' iter pp2
     in (n' + 1, pp3)
  Atomic op -> opToPoint kenv pid (n, pp) op

opsToPoints :: KEnv -> Pid -> Config -> [Op] -> Config
opsToPoints kenv pid (n, pp) = Prelude.foldl (opToPoint kenv pid) (n, pp)

opToPoint :: KEnv -> Pid -> Config -> Op -> Config
opToPoint kenv pid (n, pp) op =
  let c = chName op
      ins guard inc =
        let body =
              T.Block
                [ -- c := c {+,-} 1
                  T.Assign [(c, inc (T.EVar c) (T.ECon (T.CNum 1)))],
                  -- p := n + 1
                  T.Assign [((pid <|), T.ECon (T.CNum (n + 1)))]
                ]
            i = T.If guard body Nothing
         in (n + 1, M.insert n i pp)
   in case op of
        Send _ ->
          let k = Data.Maybe.fromJust (M.lookup c kenv)
              -- c < cap(c)
              guard = T.Lt (T.EVar c) k
           in ins guard T.Plus
        Recv _ ->
          let -- c > 0
              guard = T.Gt (T.EVar c) (T.ECon (T.CNum 0))
           in ins guard T.Minus