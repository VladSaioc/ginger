module Pipeline.IRTranslation.Utilities where

import Backend.Ast qualified as T
import Backend.Utilities
import IR.Ast
import Data.Map qualified as M
import Utilities.Collection

-- | An alias for the type of process ids. Its purpose is to provide
-- clarity to type definitions involving process ids.
type P = Int

-- | An alias for the type of program points. Its purpose is to provide
-- clarity to type definitions involving program points.
type 𝑁 = Int
-- | The pattern for the unspawned goroutine program point.
pattern UNSPAWNED :: (Eq a, Num a) => a
pattern UNSPAWNED = -1
-- | The program point of unspawned goroutines as a value.
_UNSPAWNED :: Int
_UNSPAWNED = -1
-- | The pattern for the crashed goroutine program point.
pattern CRASHED :: (Eq a, Num a) => a
pattern CRASHED = -2
-- | The program point of crashed goroutines as a value.
_CRASHED :: Int
_CRASHED = -2


-- | Bindings from program points to statements, encoding the semantics
-- of the operation at the given point.
type 𝛷 = 𝑁 ↦ T.Stmt

-- | Bindings from process ids to program points.
type 𝛯 = P ↦ 𝛷

-- | Bindings from process ids to program point reachability conditions.
type 𝛹 = P ↦ (𝑁 ↦ T.Exp)

-- | A process traversal context. When performing traversal on the IR program
-- such that it knows:
-- 1. The current process ID.
-- 2. The next fresh process ID.
-- 3. The next program point.
data 𝛬 = 𝛬 {
  p :: P,
  nextp :: P,
  𝑛 :: 𝑁
}

-- | Program counter variable name. Produces the variable storing program
-- counters for each process. Given process id p, the naming schema is
-- (contents between braces are swapped with the variable contents):
--
-- > P{p}
(⊲) :: P -> String
(⊲) p = "P" ++ show p

-- | Program id to exit variable name. Produces the variable storing program
-- exit points for each process. Given process id p, the naming schema is
-- (contents between braces are swapped with the variable contents):
--
-- > X{p}
(▽) :: P -> String
(▽) p = "X" ++ show p

-- | Annotate process-local variable. Given process id p and name x,
-- the naming schema is (contents between braces are swapped with
-- the variable contents):
--
-- > P{p}'{x}
(%) :: P -> 𝑋 -> 𝑋
(%) p x = (p ⊲) ++ "'" ++ x

-- | Program id to program counter variable expression.
--
-- > P{p}
𝜋 :: P -> T.Exp
𝜋 p = ((p ⊲) @)

-- | Program id to exit variable expression.
--
-- > X{p}
𝜒 :: P -> T.Exp
𝜒 p = ((p ▽) @)

-- | Given a set of program points, produces the next available program point.
(-|) :: 𝛷 -> T.Exp
(-|) 𝜙 = case M.toDescList 𝜙 of
  [] -> (0 #)
  (𝑛, _) : _ -> (𝑛 #)

-- | Folds program to aggregate a certain collection.
programToCollection :: Collection a => (𝛬 -> 𝑆 -> a) -> 𝑃 -> a
programToCollection f (𝑃 _ s₀) =
  let foldStatement 𝜆 s =
        let 𝜆' = 𝜆 { 𝑛 = 𝑛 𝜆 + ppOffset s }
            𝜎₀ = f 𝜆 s
         in case s of
            Skip -> (𝜆', 𝜎₀)
            Return -> (𝜆', 𝜎₀)
            Close _ -> (𝜆', 𝜎₀)
            Atomic {} -> (𝜆', 𝜎₀)
            Seq s₁ s₂ ->
              let (𝜆₁, 𝜎₁) = foldStatement 𝜆 s₁
                  (𝜆₂, 𝜎₂) = foldStatement 𝜆₁ s₂
               in (𝜆₂, 𝜎₁ ∪ 𝜎₂ ∪ 𝜎₀)
            If _ s₁ s₂ ->
              let (𝜆₁, 𝜎₁) = foldStatement 𝜆 { 𝑛 = 𝑛 𝜆 + 1 } s₁
                  (𝜆₂, 𝜎₂) = foldStatement 𝜆₁ { 𝑛 = 𝑛 𝜆₁ + 1 } s₂
               in (𝜆₂ { 𝑛 = 𝑛 𝜆₂ }, 𝜎₀ ∪ 𝜎₁ ∪ 𝜎₂)
            For {} -> (𝜆', 𝜎₀)
            Go s₁ ->
              let (𝜆₁, 𝜎₁) = foldStatement 𝛬 { 𝑛 = 0, p = nextp 𝜆, nextp = nextp 𝜆 + 1 } s₁
               in (𝜆' { nextp = nextp 𝜆₁}, 𝜎₀ ∪ 𝜎₁)
   in snd $ foldStatement 𝛬 { 𝑛 = 0, p = 0, nextp = 1} s₀
