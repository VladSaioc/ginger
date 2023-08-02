module Pipeline.IRTranslation.Boilerplate (wholeEncoding) where

import Backend.Ast
import Backend.Utilities
import Data.List qualified as L
import Data.Map qualified as M
import Pipeline.IRTranslation.CapPrecondition (capPreconditions)
import Pipeline.IRTranslation.CommPrecondition (preconditions)
import Pipeline.IRTranslation.Enabled (enabledExp)
import Pipeline.IRTranslation.Invariant.ChannelBound (channelBounds)
import Pipeline.IRTranslation.Invariant.ChannelMonitor (channelMonitors)
import Pipeline.IRTranslation.Invariant.CounterBound (counterInvariants)
import Pipeline.IRTranslation.Invariant.If (ifMonitors)
import Pipeline.IRTranslation.Invariant.Loop (loopMonitors)
import Pipeline.IRTranslation.Invariant.RendezvousMutex (rendezvousMutexes)
import Pipeline.IRTranslation.Invariant.RendezvousNoAsync (noAsyncRendezvous)
import Pipeline.IRTranslation.Meta.Channel
import Pipeline.IRTranslation.Meta.If
import Pipeline.IRTranslation.Meta.Loop
import Pipeline.IRTranslation.Postcondition (postconditions)
import Pipeline.IRTranslation.Utilities

{- A function for computing the number of iterations that
may be performed in a loop.

Produces:
ghost function iterations(lo, hi) : int {
  if lo <= hi then hi - lo else 0
}
-}
iterationsFunc :: Function
iterationsFunc =
  let lo = ("lo" @)
      hi = ("hi" @)
   in Function
        { yields = TInt,
          funcBody = IfElse (lo :<= hi) (hi :- lo) (0 #),
          funcHoare =
            HoareWrap
              { ghost = True,
                name = "iter",
                types = [],
                params = [("lo", TInt), ("hi", TInt)],
                requires = [],
                ensures = [],
                decreases = []
              }
        }

{- A predicate on schedules that ensures all natural numbers
are bound to valid process IDs.
Depends on: Π

Produces:
ghost function isSchedule(S : nat -> nat) {
  forall n :: s <= |dom(Π)|
}
-}
isScheduleFunc :: 𝛱 -> Function
isScheduleFunc ps =
  let n = "n"
      domPi = ((M.size ps - 1) #)
      callS = Call "S" [(n @)]
   in Function
        { yields = TBool,
          funcBody = Forall [(n, Nothing)] (callS :<= domPi),
          funcHoare =
            HoareWrap
              { ghost = True,
                name = "isSchedule",
                types = [],
                params = [("S", TNat :-> TNat)],
                requires = [],
                ensures = [],
                decreases = []
              }
        }

{- Case analysis of a single process over its program points.
Depends on: π, 𝜙

Produces:
switch pc(π) {
  ∀ n ∈ 𝜙. case n => 𝜙(n)
}
-}
processSwitch :: P -> 𝛷 -> Stmt
processSwitch pid =
  let pc = π pid
      iguard c = PCon (CNum c)
      cases = M.toList . M.mapKeys iguard
   in MatchStmt pc . cases

{- Case analysis for scheduled process at the given step
over process ids.
Depends on: Π

Produces:
switch S(step) {
  ∀ π ∈ Π. case π => processSwitch(π, 𝜙)
}
-}
scheduleSwitch :: 𝛱 -> Stmt
scheduleSwitch =
  let iguard pid = PCon (CNum pid)
      cases = M.toList . M.mapKeys iguard . M.mapWithKey processSwitch
      step = Call "S" [("step" @)]
   in MatchStmt step . cases

{- Constructs the central loop which emulates the execution
of the concurrent program.
Depends on: κ, Π, nonloop(P), loop(P)

Produces:
while enabledExp(κ, Π)
∀ (π, 𝜙) ∈ Π. invariant counterInvariant(π, 𝜙)
∀ e ∈ channelMonitors(noloop(P), loop(P)). invariant e
∀ l ∈ loop(P). invariant loopMonitor(l)
{
  scheduleSwitch(Π)
  step := step + 1
}
-}
centralLoop :: K -> 𝛱 -> P ↦ (𝐶 ↦ 𝒪s) -> [ℐ] -> [ℒ] -> Stmt
centralLoop κ ps atomicOps ifs loops =
  let -- If statement invariants
      i = ifMonitors ifs
      -- Process loop invariants
      l = loopMonitors loops
      -- Channel bound invariants
      k = channelBounds κ
      -- Absence of rendezvous for buffered channels invariants
      rv = noAsyncRendezvous κ atomicOps loops
      -- Mutual exclusion between rendezvous points of different process
      -- on the same channel
      rvm = rendezvousMutexes ps
      -- Process counter invariants
      pc = counterInvariants ps
      -- Channel buffer size invariants
      m = channelMonitors κ atomicOps loops
      -- Condition under which progress is enabled
      -- 1. Fuel constraint
      hasFuel = ("step" @) :< ("fuel" @)
      -- 2. Fuel + process operation disjunctions
      enabled = hasFuel :&& enabledExp κ ps
   in While
        enabled
        (concat [k, pc, rv, rvm, i, l, m])
        []
        ( Block
            [ -- Central loop case analysis
              scheduleSwitch ps,
              -- Increment steps
              Assign [("step", ("step" @) :+ (1 #))]
            ]
        )

{- Constructs an initial assignment for all program counters.
Depends on: Π

Produces:
var pc(π)₁, ..., pc(π)ₙ = 0, ..., 0
-}
counterDef :: 𝛱 -> Stmt
counterDef ps =
  if M.size ps > 0
    then
      let def p = ((p ⊲), (0 #))
       in Assign . L.map def . M.keys $ ps
    else Assert (True ?)

{- Constructs an initial assignment for all loop variables.
Depends on: loop(P)

Produces:
var x₁, ..., xₙ = lo₁, ..., loₙ
-}
loopVarDef :: [ℒ] -> Stmt
loopVarDef = \case
  [] -> Assert (True ?)
  ls ->
    let def (ℒ {l𝑋 = x, lower}) = (x, Nothing, lower)
     in VarDef False . L.map def $ ls

{- Constructs an initial assignment for all channel variables.
Depends on: κ

Produces:
var c₁, ..., cₙ = 0, ..., 0
-}
chanDef :: K -> Stmt
chanDef κ =
  if M.size κ > 0
    then
      let def c = (c, Nothing, (0 #))
       in VarDef False . L.map def . M.keys $ κ
    else Assert (True ?)

{- Construcs the "isSchedule(S)" precondition.
-}
isSchedule :: Exp
isSchedule = Call "isSchedule" [("S" @)]

{- Constructs the main program encoding.
Depends on: κ, Π, nonloop(P), loop(P), fv(P)

Produces:
method Program(S : nat -> nat, ∀ x ∈ fv(P). x : int)
returns (∀ (π, 𝜙) ∈ Π. pc(π) : int)

requires capPreconditions(κ)
requires preconditions(κ, nonloop(P), loop(P))

ensures postconditions(Π)

decreases * {
  counterDef(Π);
  chanDef(κ);
  loopVarDef(loop(P));
  step := 0;
  centralLoop(κ, Π, nonloop(P), loop(P))
}

-}
progEncoding :: 𝛴 -> [Type] -> K -> 𝛱 -> P ↦ (𝐶 ↦ 𝒪s) -> [ℐ] -> [ℒ] -> Method
progEncoding 𝜎 ts κ ps os ifs loops =
  Method
    { returns = ("step", TNat) : (L.map ((,TInt) . (⊲)) . M.keys) ps,
      methodHoare =
        HoareWrap
          { ghost = True,
            name = "Program",
            types = ts,
            params = ("fuel", TNat) : ("S", TNat :-> TNat) : M.toList 𝜎,
            ensures =
              [ (("step" @) :< ("fuel" @)) :==> ((preconditions κ os loops ...⋀) :<==> (postconditions ps ...⋀))
              ],
            decreases = [],
            requires = isSchedule : capPreconditions κ
          },
      methodBody =
        Block
          [ counterDef ps,
            chanDef κ,
            loopVarDef loops,
            Assign [("step", (0 #))],
            centralLoop κ ps os ifs loops
          ]
    }

{- Constructs the complete program specification, by emitting
all the necessary functions, and the program encoding.
-}
wholeEncoding :: 𝛴 -> [Type] -> K -> 𝛱 -> P ↦ (𝐶 ↦ 𝒪s) -> [ℐ] -> [ℒ] -> Program
wholeEncoding 𝜎 ts κ ps os is ls =
  Program
    [ FDecl iterationsFunc,
      FDecl (isScheduleFunc ps),
      MDecl (progEncoding 𝜎 ts κ ps os is ls)
    ]