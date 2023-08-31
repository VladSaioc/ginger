module Pipeline.IRTranslation.Reachability (reachability) where

import Backend.Ast qualified as T
import Backend.Utilities
import Data.Map qualified as M
import IR.Ast
import Pipeline.IRTranslation.Exps
import Pipeline.IRTranslation.Utilities
import Utilities.Collection

{- | Computes reachability conditions for all processes in the program.
-}
reachability :: 𝑃 -> P ↦ (𝑁 ↦ T.Exp)
reachability (𝑃 _ ps) =
  let ps' = zip [0 ..] ps
   in M.mapWithKey (const pointReachability) $ M.fromList ps'

{- | Creates an intra-processual reachability condition
map for every instruction point in a process.

For individual statements, it produces a binding from program points
to reachability conditions, given an instruction point.
Its return context also includes the instruction point offset
for possible continuations, and the aggregated condition leading to a return
statement.

Rules:

> [RETURN]: ⟨e, 𝑛 : return⟩ -> ⟨𝑛 + 1, e : [𝑛 ↦ e]⟩

> [SKIP]:   ⟨_, 𝑛 : skip⟩ -> ⟨𝑛, false : []⟩

> [FOR]:    ⟨_, 𝑛 : for _ _ os⟩ -> ⟨𝑛ₙ + 1, false : [𝑛 ↦ e]⟩
>           |- 𝑛₀ = 𝑛 + 1
>           |- ∀ 0 ≤ i < n, n = |os|, oᵢ ∈ os, 𝜓ᵢ = [𝑛ᵢ ↦ e].
>               ⟨e, 𝑛ᵢ : oᵢ⟩ -> ⟨𝑛ᵢ₊₁, false : 𝜓ᵢ⟩
>           |- 𝜓 = ⋃ ∀ 0 ≤ i < n. 𝜓ᵢ

> [SEND]:   ⟨e, 𝑛 : c!⟩ -> ⟨𝑛 + 2, false : [𝑛 ↦ e]⟩

> [RECV]:   ⟨e, 𝑛 : c?⟩ -> ⟨𝑛 + 1, false : [𝑛 ↦ e]⟩

> [SEQ]:    ⟨e, 𝑛 : S₁; S₂⟩ -> ⟨𝑛₂, e₁ || e₂ : 𝜓₁ ∪ 𝜓₂⟩
>           |- ⟨e, 𝑛, S₁⟩ -> ⟨𝑛₁, e₁ : 𝜓₁⟩
>           |- ⟨!e₁ && e, 𝑛₁, S₂⟩ -> ⟨𝑛₂, e₂ : 𝜓₂⟩

> [IF]:     ⟨e, 𝑛 : if e' then S₁ else S₂⟩ -> ⟨𝑛₂, e₁ || e₂ : 𝜓₁ ∪ 𝜓₂⟩
>           |- ⟨e && e', 𝑛 : S₁⟩ -> ⟨𝑛₁, e₁ : 𝜓₁⟩
>           |- ⟨e && !e', S₂⟩ -> ⟨𝑛₂, e₂ : 𝜓₂⟩
-}
pointReachability :: 𝑆 -> 𝑁 ↦ T.Exp
pointReachability =
  let pointReachability' e (𝑛, s) = case s of
        -- Skip statements do not increment the program counter,
        -- and do not result in an early return condition.
        Skip -> (M.empty, 𝑛, (False ?))
        -- Return statements depend
        Return -> (M.insert 𝑛 e M.empty, 𝑛 + 1, e)
        Seq s₁ s₂ ->
          let (𝜓₁, 𝑛₁, e₁) = pointReachability' e (𝑛, s₁)
              (𝜓₂, 𝑛₂, e₂) = pointReachability' (T.Not e₁ T.:&& e) (𝑛₁, s₂)
           in (M.union 𝜓₁ 𝜓₂, 𝑛₂, e₁ T.:|| e₂)
        For _ _ _ os ->
          let addOp (𝑛'', 𝜓') o = (𝑛'' + ppOffset o, M.insert 𝑛'' e 𝜓')
              (𝑛', 𝜓) = foldl addOp (𝑛 + 1, M.insert 𝑛 e M.empty) os
           in (𝜓, 𝑛' + 1, (False ?))
        Atomic {} -> (M.fromList [(𝑛, e)], 𝑛 + ppOffset s, (False ?))
        -- If statements may add additional reachability conditions to
        -- possibile return statements encountered along the branches.
        If e0 s1 s2 ->
          let e' = parseExp e0
              -- The 'then' branch extends reachability with the guard condition.
              (𝜓₁, 𝑛₁, e₁) = pointReachability' (e T.:&& e') (𝑛 + 1, s1)
              -- The 'else' branch extends reachability with the negated guard condition.
              (𝜓₂, 𝑛₂, e₂) = pointReachability' (e T.:&& T.Not e') (𝑛₁ + 1, s2)
           in -- Augment 'then' points path reachability with a negation of
              -- return statements from the else branch.
              -- 𝜓₁' = M.map (e T.:&& T.Not e₂ T.:&&) 𝜓₁
              -- 𝜓₂' = M.map (e T.:&& T.Not e₁ T.:&&) 𝜓₂

              -- Program point reachability maps are joined.
              -- The if exit point is the continuation point of the else branch.
              -- Return conditions are a disjunction between return conditions
              -- of the branches.
              (M.union 𝜓₁ 𝜓₂, 𝑛₂, e₁ T.:|| e₂)
   in (\(𝜓, _, _) -> 𝜓)
        . pointReachability' (True ?)
        . (0,)
