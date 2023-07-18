module Pipeline.IRTranslation.Processes (getProcs) where

import Backend.Ast qualified as T
import Backend.Utilities
import Data.Map qualified as M
import Data.Maybe qualified
import IR.Ast
import IR.Utilities
import Pipeline.IRTranslation.Exps
import Pipeline.IRTranslation.Utilities

-- Configurations include a binding for program points achieved so far,
-- and the next available program point to be used by a fresh instruction.
type Config = (Int, ProgPoints)

{- Transforms a IR program intro a map from process ids to program points.
Depends on: κ, P = S₁, ..., Sₙ

Produces: Π = [ πᵢ ↦ ϕᵢ | ϕᵢ = stmtsToPoints(κ, πᵢ, ⟨0, []⟩, Sᵢ) ]
-}
getProcs :: KEnv -> Prog -> Procs
getProcs kenv (Prog _ prcs) =
  let pidsAndSyntax = zip (take (length prcs) [0 ..]) prcs
      makeProc (pid, stmt) =
        let (n, pp) = stmtToPoints kenv pid (0, M.empty) stmt
            pp' = M.insert n (T.Block []) pp
         in (pid, pp')
   in M.fromList (map makeProc pidsAndSyntax)

{- Transform an IR statement into a map of program points.
Depends on: κ, π, ⟨n, ϕ⟩, S

Produces, based on S:
1. [SKIP]: skip -> ⟨n, ϕ⟩
2. [COMM]: c{!,?} -> opToPoints(κ, π, ⟨n, ϕ⟩, c{!,?})
3. [SEQ]: S₁; S₂ -> ⟨n', ϕ'⟩
          |- S₁ -> ⟨n'', ϕ''⟩
          |- S₂ -> ⟨n', ϕ'⟩
4. [FOR]: for (i : e₁ .. e₂) { s } -> ⟨n' + 1, ϕ''⟩
          |- ⟨n', ϕ'⟩ = opToPoints(κ, π, ⟨n + 1, ϕ⟩, s)
          |- ϕ'' = ϕ'[
            n ↦ if x < e₂ {
                pc(π) := n + 1
              } else {
                pc(π) := n' + 1
              },
            n' ↦ {
              x := x + 1;
              pc(π) := n;
            }
          ]
-}
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

{- Updates a program point set with the translations of
  the operation in the provided sequence.
-}
opsToPoints :: KEnv -> Pid -> Config -> [Op] -> Config
opsToPoints kenv pid (n, pp) = Prelude.foldl (opToPoint kenv pid) (n, pp)

{- Appends a set of program points with a new program point,
based on the next available instruction.
Depends on: κ, π, ⟨n, ϕ⟩, o

Produces:
1. If o = c!, then:
  ⟨n + 2, ϕ = [
    n ↦ if 0 < κ(c) {
        if c < κ(c) {
          c := c + 1;
          pc(π) := n + 2;
        }
      } else {
        if c == 0 {
          c := 1;
          pc(π) := n + 1;
        }
      }
    (n + 1) ↦ if c == 1 {
        c := -1;
        pc(π) := n + 2;
      }
  ]⟩
1. If o = c?, then:
  ⟨n + 1, ϕ = [
    n ↦ if 0 < κ(c) {
        if c > 0 {
          c := c - 1;
          pc(π) := n + 1;
        }
      } else {
        if c == 1 {
          c := -1;
          pc(π) := n + 1;
        }
      }
  ]⟩
-}
opToPoint :: KEnv -> Pid -> Config -> Op -> Config
opToPoint kenv pid (n, pp) op =
  let c = chName op
      -- pc(π) = n'
      nextInstruction n' = T.Assign [((pid <|), (n' #))]
      -- if g { b }
      ifNoElse g b = T.If g (T.Block b) Nothing
      -- κ(c)
      k = Data.Maybe.fromJust (M.lookup c kenv)
      -- if 0 < κ(c) { s1 } else { s2 }
      syncPoint s1 s2 =
        let wrap = T.If (T.Lt (0 #) k)
         in -- Ensure statements are wrapped in blocks
            case (s1, s2) of
              (T.Block _, T.Block _) -> wrap s1 (return s2)
              (T.Block _, _) -> wrap s1 (return $ T.Block [s2])
              (_, T.Block _) -> wrap (T.Block [s1]) (return s2)
              (_, _) -> wrap (T.Block [s1]) (return $ T.Block [s2])
      -- c := e
      assignChan e = T.Assign [(c, e)]
      -- Guarded channel operation for unbuffered communication
      sync n' current new =
        let body = [assignChan (new #), nextInstruction n']
         in ifNoElse (T.Eq (c @) (current #)) body
      -- Guarded channel operation for buffered communication
      async n' guard inc =
        let body =
              [ -- c := c {+,-} 1
                assignChan $ inc (c @) (1 #),
                -- p := n + 1
                nextInstruction n'
              ]
         in ifNoElse guard body
   in case op of
        Send _ ->
          let -- c < κ(c)
              guard = T.Lt (c @) k
              -- if c < κ(c) { c := c + 1; p := n + 2 }
              asyncCase = async (n + 2) guard T.Plus
              -- if c == 0 { c := 1; p := n + 1 }
              syncCase = sync (n + 1) 0 1
              -- if 0 < κ(c) { <async case> } else { <sync case> }
              opPoint = syncPoint asyncCase syncCase
              -- if c == -1 { c := 0; p := n + 2 }
              rendezvousPoint = sync (n + 2) (-1) 0
              -- Insert send operation at program point n
              pp' = M.insert n opPoint pp
              -- Insert rendezvous at program point n+1
              pp'' = M.insert (n + 1) rendezvousPoint pp'
           in -- Return program points and next available instruction
              -- point n+2
              (n + 2, pp'')
        Recv _ ->
          let -- c > 0
              guard = T.Gt (c @) (0 #)
              -- if c > 0 { c := c - 1; p := n + 1 }
              asyncCase = async (n + 1) guard T.Minus
              -- if c == 1 { c := -1; p := n + 1 }
              syncCase = sync (n + 1) 1 (-1)
              -- if 0 < κ(c) { <async case> } else { <sync case> }
              opPoint = syncPoint asyncCase syncCase
              -- Insert receive operation at program point n
              pp' = M.insert n opPoint pp
           in -- Return program points and next available instruction
              -- point n+1
              (n + 1, pp')