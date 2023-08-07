module Pipeline.IRTranslation.Processes (getProcs) where

import Backend.Ast qualified as T
import Backend.Utilities
import Data.Map qualified as M
import Data.Maybe qualified
import IR.Ast
import IR.Utilities
import Pipeline.IRTranslation.Exps
import Pipeline.IRTranslation.Meta.Channel
import Pipeline.IRTranslation.Utilities

{- Transforms a IR program intro a map from process ids to program points.
Depends on: κ, P = S₁, ..., Sₙ

Produces: 𝛱 = [ πᵢ ↦ 𝜙ᵢ | 𝜙ᵢ = stmtsToPoints(κ, πᵢ, ⟨0, []⟩, Sᵢ) ]
-}
getProcs :: K -> 𝑃 -> 𝛱
getProcs κ (𝑃 _ ss) =
  let pidsAndSyntax = zip (take (length ss) [0 ..]) ss
      makeProc (p, stmt) =
        let (𝑛, 𝜙) = stmtToPoints κ p (0, M.empty) stmt
            𝜙' = M.insert 𝑛 (T.Block []) 𝜙
         in (p, 𝜙')
   in M.fromList (map makeProc pidsAndSyntax)

{- Transform an IR statement into a map of program points.
Depends on: κ, π, ⟨𝑛, 𝜙⟩, S

Produces, based on S:
1. [SKIP]: skip -> ⟨𝑛, 𝜙⟩
2. [COMM]: c{!,?} -> opToPoints(κ, π, ⟨𝑛, 𝜙⟩, c{!,?})
3. [SEQ]: S₁; S₂ -> ⟨𝑛', 𝜙'⟩
          |- S₁ -> ⟨𝑛'', 𝜙''⟩
          |- S₂ -> ⟨𝑛', 𝜙'⟩
4. [FOR]: for (i : e₁ .. e₂) { s } -> ⟨𝑛' + 1, 𝜙''⟩
          |- ⟨𝑛', 𝜙'⟩ = opToPoints(κ, π, ⟨𝑛 + 1, 𝜙⟩, s)
          |- 𝜙'' = 𝜙'[
            𝑛 ↦ if x < e₂ {
                pc(π) := 𝑛 + 1
              } else {
                pc(π) := 𝑛' + 1
              },
            𝑛' ↦ {
              x := x + 1;
              pc(π) := 𝑛;
            }
          ]
-}
stmtToPoints :: K -> P -> (𝑁, 𝛷) -> 𝑆 -> (𝑁, 𝛷)
stmtToPoints κ p (𝑛, 𝜙) =
  let moveTo 𝑛' is =
        T.Block
          (T.Assign [((p ⊲), (𝑛' #))] : is)
   in \case
        Skip -> (𝑛, 𝜙)
        Return ->
          let exit = T.Block [T.Assign [((p ⊲), 𝜒 p)]]
              𝜙' = M.insert 𝑛 exit 𝜙
           in (𝑛 + 1, 𝜙')
        Seq s1 s2 ->
          let (𝑛', 𝜙') = stmtToPoints κ p (𝑛, 𝜙) s1
           in stmtToPoints κ p (𝑛', 𝜙') s2
        If e s1 s2 ->
          let -- Translate guard expression
              e' = parseExp e
              -- Translate then branch
              (𝑛₁, 𝜙₁) = stmtToPoints κ p (𝑛 + 1, 𝜙) s1
              -- Translate else branch
              (𝑛₂, 𝜙₂) = stmtToPoints κ p (𝑛₁ + 1, 𝜙₁) s2
              -- if e' { pc := 𝑛 + 1 } else { pc := 𝑛'' }
              thn = moveTo (𝑛 + 1) []
              els = moveTo (𝑛₁ + 1) []
              guard = T.If e' thn (Just els)
              -- { pc := 𝑛' }
              leaveThn = moveTo 𝑛₂ []
              𝜙₃ = M.insert 𝑛 guard 𝜙₂
              𝜙₄ = M.insert 𝑛₁ leaveThn 𝜙₃
           in (𝑛₂, 𝜙₄)
        For x _ e ops ->
          let x' = p % x
              e' = parseExp e
              (𝑛', 𝜙₁) = opsToPoints κ p (𝑛 + 1, 𝜙) ops

              -- x < e
              guard = (x' @) T.:< e'
              -- { pc := 𝑛 + 1 }
              stay = moveTo (𝑛 + 1) []
              -- { pc := 𝑛' + 1 }
              leave = moveTo (𝑛' + 1) []
              -- { x := x + 1; pc := 𝑛 }
              iter = moveTo 𝑛 [T.Assign [(x', (x' @) T.:+ (1 #))]]

              -- 𝜙₂ = 𝜙[𝑛 ↦ if x < e { pc := 𝑛 + 1; } else { pc := 𝑛' + 1 }]
              𝜙₂ = M.insert 𝑛 (T.If guard stay (Just leave)) 𝜙₁
              -- 𝜙₃ = 𝜙[𝑛' ↦ { x := x + 1; pc := 𝑛 }]
              𝜙₃ = M.insert 𝑛' iter 𝜙₂
           in (𝑛' + 1, 𝜙₃)
        Atomic op -> opToPoint κ p (𝑛, 𝜙) op

{- Updates a program point set with the translations of
  the operation in the provided sequence.
-}
opsToPoints :: K -> P -> (𝑁, 𝛷) -> [Op] -> (𝑁, 𝛷)
opsToPoints κ p (𝑛, 𝜙) = Prelude.foldl (opToPoint κ p) (𝑛, 𝜙)

{- Appends a set of program points with a new program point,
based on the next available instruction.
Depends on: κ, π, ⟨n, 𝜙⟩, o

Produces:
1. If o = c!, then:
  ⟨n + 2, 𝜙 = [
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
  ⟨n + 1, 𝜙 = [
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
opToPoint :: K -> P -> (𝑁, 𝛷) -> Op -> (𝑁, 𝛷)
opToPoint κ p (𝑛, 𝜙) op =
  let c = chName op
      -- pc(π) = n'
      nextInstruction 𝑛' = T.Assign [((p ⊲), (𝑛' #))]
      -- if g { b }
      ifNoElse g b = T.If g (T.Block b) Nothing
      -- κ(c)
      k = Data.Maybe.fromJust (M.lookup c κ)
      -- if 0 < κ(c) { s1 } else { s2 }
      syncPoint s1 s2 =
        let wrap = T.If ((0 #) T.:< k)
         in -- Ensure statements are wrapped in blocks
            case (s1, s2) of
              (T.Block _, T.Block _) -> wrap s1 (return s2)
              (T.Block _, _) -> wrap s1 (return $ T.Block [s2])
              (_, T.Block _) -> wrap (T.Block [s1]) (return s2)
              (_, _) -> wrap (T.Block [s1]) (return $ T.Block [s2])
      -- c := e
      assignChan e = T.Assign [(c, e)]
      -- Guarded channel operation for unbuffered communication
      sync 𝑛' current new =
        let body = [assignChan (new #), nextInstruction 𝑛']
         in ifNoElse ((c @) T.:== (current #)) body
      -- Guarded channel operation for buffered communication
      async 𝑛' guard inc =
        let body =
              [ -- c := c {+,-} 1
                assignChan $ inc (c @) (1 #),
                -- p := n + 1
                nextInstruction 𝑛'
              ]
         in ifNoElse guard body
   in case op of
        Send _ ->
          let -- c < κ(c)
              guard = (c @) T.:< k
              -- if c < κ(c) { c := c + 1; p := n + 2 }
              asyncCase = async (𝑛 + 2) guard (T.:+)
              -- if c == 0 { c := 1; p := n + 1 }
              syncCase = sync (𝑛 + 1) 0 1
              -- if 0 < κ(c) { <async case> } else { <sync case> }
              opPoint = syncPoint asyncCase syncCase
              -- if c == -1 { c := 0; p := n + 2 }
              rendezvousPoint = sync (𝑛 + 2) (-1) 0
              -- Insert send operation at program point n
              𝜙' = M.insert 𝑛 opPoint 𝜙
              -- Insert rendezvous at program point n+1
              𝜙̋₂ = M.insert (𝑛 + 1) rendezvousPoint 𝜙'
           in -- Return program points and next available instruction
              -- point n+2
              (𝑛 + 2, 𝜙̋₂)
        Recv _ ->
          let -- c > 0
              guard = (c @) T.:> (0 #)
              -- if c > 0 { c := c - 1; p := n + 1 }
              asyncCase = async (𝑛 + 1) guard (T.:-)
              -- if c == 1 { c := -1; p := n + 1 }
              syncCase = sync (𝑛 + 1) 1 (-1)
              -- if 0 < κ(c) { <async case> } else { <sync case> }
              opPoint = syncPoint asyncCase syncCase
              -- Insert receive operation at program point n
              𝜙' = M.insert 𝑛 opPoint 𝜙
           in -- Return program points and next available instruction
              -- point n+1
              (𝑛 + 1, 𝜙')