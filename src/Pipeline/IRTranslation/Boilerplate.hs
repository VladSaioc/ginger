module Pipeline.IRTranslation.Boilerplate (wholeEncoding) where

import Backend.Ast
import Backend.Utilities
import Data.List qualified as L
import Data.Map qualified as M
import Pipeline.IRTranslation.Clauses.CapPrecondition (capPreconditions)
import Pipeline.IRTranslation.Clauses.CommPrecondition (preconditions)
import Pipeline.IRTranslation.Enabled (enabledExp)
import Pipeline.IRTranslation.Invariant.ChannelBound (channelBounds)
import Pipeline.IRTranslation.Invariant.ChannelMonitor (channelMonitors)
import Pipeline.IRTranslation.Invariant.CounterBound (counterInvariants)
import Pipeline.IRTranslation.Invariant.If (ifMonitors)
import Pipeline.IRTranslation.Invariant.Loop (loopMonitors)
import Pipeline.IRTranslation.Invariant.Go (goMonitors)
import Pipeline.IRTranslation.Invariant.RendezvousMutex (rendezvousMutexes)
import Pipeline.IRTranslation.Invariant.RendezvousNoAsync (noAsyncRendezvous)
import Pipeline.IRTranslation.Invariant.Return (returnMonitors)
import Pipeline.IRTranslation.Meta.Channel
import Pipeline.IRTranslation.Meta.Go
import Pipeline.IRTranslation.Meta.If
import Pipeline.IRTranslation.Meta.Loop
import Pipeline.IRTranslation.Meta.Return
import Pipeline.IRTranslation.Clauses.Postcondition (postconditions)
import Pipeline.IRTranslation.Utilities
import Utilities.Collection

{- | A function for computing the number of iterations that
may be performed in a loop.

Produces:

> ghost function iterations(lo, hi) : int {
>   if lo <= hi then hi - lo else 0
> }
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

{- | A predicate on schedules that ensures all schedule steps
are bound to valid process IDs.
Depends on: 𝛯

Produces:

> ghost function isSchedule(S : nat -> nat) {
>   forall n :: s <= |dom(𝛯)|
> }
-}
isScheduleFunc :: 𝛯 -> Function
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

{- | Case analysis of a single process over its program points.
Depends on: p, 𝜙

Produces:

> switch 𝜋(p) {
>   ∀ 𝑛 ∈ 𝜙. case n => 𝜙(𝑛)
> }
-}
processSwitch :: P -> 𝛷 -> Stmt
processSwitch pid =
  let pc = 𝜋 pid
      iguard c = PCon (CNum c)
      cases = M.toList . M.mapKeys iguard
   in MatchStmt pc . cases

{- | Case analysis for scheduled process at the given step
over process ids.
Depends on: 𝛯

Produces:

> switch S(step) {
>   ∀ p ∈ dom(𝛯). case p => processSwitch(p, 𝜙)
> }
-}
scheduleSwitch :: 𝛯 -> Stmt
scheduleSwitch =
  let iguard pid = PCon (CNum pid)
      cases = M.toList . M.mapKeys iguard . M.mapWithKey processSwitch
      step = Call "S" [("step" @)]
   in MatchStmt step . cases

{- | Constructs the central loop which emulates the execution
of the concurrent program.
Depends on: 𝜓, κ, 𝛯, nonloop(P), loop(P)

Produces:

> while enabledExp(κ, 𝛯)
> ∀ (p, 𝜙) ∈ 𝛯. invariant counterInvariant(p, 𝜙)
> ∀ e ∈ channelMonitors(noloop(P), loop(P)). invariant e
> ∀ ℓ ∈ loop(P). invariant loopMonitor(ℓ)
> {
>   scheduleSwitch(𝛯)
>   step := step + 1
> }
-}
centralLoop :: 𝛹 -> K -> 𝛯 -> P ↦ (𝐶 ↦ 𝒪s) -> [𝒢] -> [ℐ] -> [ℒ] -> [ℛ] -> Stmt
centralLoop 𝜓 κ ps atomicOps gs ifs ls rs =
  let -- Go statement invariants
      g = goMonitors 𝜓 gs
      -- If statement invariants
      i = ifMonitors ifs
      -- Process loop invariants
      l = loopMonitors 𝜓 ls
      -- Return statement invariants
      r = returnMonitors 𝜓 rs
      -- Channel bound invariants
      k = channelBounds κ
      -- Absence of rendezvous for buffered channels invariants
      rv = noAsyncRendezvous κ atomicOps ls
      -- Mutual exclusion between rendezvous points of different process
      -- on the same channel
      rvm = rendezvousMutexes ps
      -- Process counter invariants
      pc = counterInvariants ps
      -- Channel buffer size invariants
      m = channelMonitors 𝜓 κ atomicOps ls
      -- Condition under which progress is enabled
      -- 1. Fuel constraint
      hasFuel = ("step" @) :< ("fuel" @)
      -- 2. Fuel + process operation disjunctions
      enabled = hasFuel :&& enabledExp κ ps
   in While
        enabled
        (concat [k, pc, rv, rvm, g, i, l, r, m])
        []
        ( Block
            [ -- Central loop case analysis
              scheduleSwitch ps,
              -- Increment steps
              Assign [("step", ("step" @) :+ (1 #))]
            ]
        )

{- | Constructs an initial assignment for all program counters.
Depends on: 𝛯

Produces:

> var 𝜋(p₁), ..., 𝜋(pₙ) = 0, ..., 0
-}
counterDef :: 𝛯 -> Stmt
counterDef ps =
  if M.size ps > 0
    then
      let def p = ((p ⊲), ((if p == 0 then 0 else -1) #))
       in Assign . L.map def . M.keys $ ps
    else Assert (True ?)

{- | Constructs an initial assignment for all loop variables.
Depends on: loop(P)

Produces:

> var x₁, ..., xₙ = lo₁, ..., loₙ
-}
loopVarDef :: [ℒ] -> Stmt
loopVarDef = \case
  [] -> Assert (True ?)
  ls ->
    let def (ℒ {l𝑋 = x, lower}) = (x, Nothing, lower)
     in VarDef False . L.map def $ ls

{- | Constructs an assignment for the process termination variables.
The variables are assigned the process termination point.
Other expressions may indirectly reference process termination
by proxy of these variables.
Depends on: 𝛯

Produces:

> ∀ (p, 𝜙) ∈ 𝛯. var 𝜒(p) = (max ∘ dom)(𝜙)
-}
terminationVars :: 𝛯 -> Stmt
terminationVars ps =
  if M.size ps > 0
    then
      let def p 𝜙 = ((p ▽), Nothing, (𝜙 -|))
       in VarDef False . M.elems . M.mapWithKey def $ ps
    else Assert (True ?)

{- | Constructs an initial assignment for all channel variables.
Depends on: κ

Produces:

> ∀ c ∈ dom(κ). var c = 0
-}
chanDef :: K -> Stmt
chanDef κ =
  if M.size κ > 0
    then
      let def c = (c, Nothing, (0 #))
       in VarDef False . L.map def . M.keys $ κ
    else Assert (True ?)

{- | Construcs the "isSchedule(S)" precondition.
-}
isSchedule :: Exp
isSchedule = Call "isSchedule" [("S" @)]

{- | Constructs the main program encoding.
Depends on: 𝜓, κ, 𝛯, ℳ

Produces:

> method Program(S : nat -> nat, ∀ x ∈ fv(P). x : int)
> returns (∀ (p, 𝜙) ∈ 𝛯. 𝜋(p) : int)
> 
> requires capPreconditions(κ)
> requires preconditions(κ, nonloop(ℳ), loop(ℳ))
> 
> ensures postconditions(𝜓, 𝛯, go(ℳ))
> 
> decreases * {
>   counterDef(𝛯);
>   terminationVars(𝛯);
>   chanDef(κ);
>   loopVarDef(loop(P));
>   step := 0;
>   centralLoop(κ, 𝛯, ℳ)
> }
-}
progEncoding ::  𝛹 -> 𝛴 -> [Type] -> K -> 𝛯 -> P ↦ (𝐶 ↦ 𝒪s) -> [𝒢] -> [ℐ] -> [ℒ] -> [ℛ] -> Method
progEncoding 𝜓 𝜎 ts κ ps os gs ifs ls rs =
  let commPreconditions = (preconditions 𝜓 κ os ls ...⋀)
   in Method
        { returns = ("step", TNat) : (L.map ((,TInt) . (⊲)) . M.keys) ps,
          methodHoare =
            HoareWrap
              { ghost = True,
                name = "Program",
                types = ts,
                params = ("fuel", TNat) : ("S", TNat :-> TNat) : M.toList 𝜎,
                ensures =
                  [ (("step" @) :< ("fuel" @)) :==> (commPreconditions :<==> (postconditions 𝜓 ps gs ...⋀))
                  ],
                decreases = [],
                requires = isSchedule : capPreconditions κ
              },
          methodBody =
            Block
              [ counterDef ps,
                terminationVars ps,
                chanDef κ,
                loopVarDef ls,
                Assign [("step", (0 #))],
                centralLoop 𝜓 κ ps os gs ifs ls rs
              ]
        }

{- | Constructs the complete program specification, by emitting
all the necessary functions, and the program encoding.
-}
wholeEncoding :: 𝛹 -> 𝛴 -> [Type] -> K -> 𝛯 -> P ↦ (𝐶 ↦ 𝒪s) -> [𝒢] -> [ℐ] -> [ℒ] -> [ℛ] -> Program
wholeEncoding 𝜓 𝜎 ts κ ps os gs is ls rs =
  Program
    [ FDecl iterationsFunc,
      FDecl (isScheduleFunc ps),
      MDecl (progEncoding 𝜓 𝜎 ts κ ps os gs is ls rs)
    ]