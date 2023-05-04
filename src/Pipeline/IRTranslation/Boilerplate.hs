module Pipeline.IRTranslation.Boilerplate (wholeEncoding) where

import Backend.Ast
import Backend.Utilities
import Data.List qualified as L
import Data.Map qualified as M
import Data.Set qualified as S
import Pipeline.IRTranslation.AsyncPrecondition (asyncPreconditions)
import Pipeline.IRTranslation.CommPrecondition (preconditions)
import Pipeline.IRTranslation.Enabled (enabledExp)
import Pipeline.IRTranslation.Invariant.ChannelBound (channelBounds)
import Pipeline.IRTranslation.Invariant.ChannelMonitor (channelMonitors)
import Pipeline.IRTranslation.Invariant.CounterBound (counterInvariants)
import Pipeline.IRTranslation.Invariant.Loop (loopMonitors)
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
        { ghost = True,
          yields = TInt,
          funcBody = IfElse (Leq lo hi) (Minus hi lo) (0 #),
          funcHoare =
            HoareWrap
              { name = "iter",
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
isScheduleFunc :: Procs -> Function
isScheduleFunc ps =
  let n = "n"
      domPi = ((M.size ps - 1) #)
      callS = Call "S" [(n @)]
   in Function
        { ghost = True,
          yields = TBool,
          funcBody = Forall [(n, Nothing)] (Leq callS domPi),
          funcHoare =
            HoareWrap
              { name = "isSchedule",
                params = [("S", Arrow TNat TNat)],
                requires = [],
                ensures = [],
                decreases = []
              }
        }

{- Case analysis of a single process over its program points.
Depends on: π, ϕ

Produces:
switch pc(π) {
  ∀ n ∈ ϕ. case n => ϕ(n)
}
-}
processSwitch :: Pid -> ProgPoints -> Stmt
processSwitch pid =
  let pc = ((pid <|) @)
      iguard c = PCon (CNum c)
      cases = M.toList . M.mapKeys iguard
   in MatchStmt pc . cases

{- Case analysis for scheduled process at the given step
over process ids.
Depends on: Π

Produces:
switch S(step) {
  ∀ π ∈ Π. case π => processSwitch(π, ϕ)
}
-}
scheduleSwitch :: Procs -> Stmt
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
∀ (π, ϕ) ∈ Π. invariant counterInvariant(π, ϕ)
∀ e ∈ channelMonitors(noloop(P), loop(P)). invariant e
∀ l ∈ loop(P). invariant loopMonitor(l)
{
  scheduleSwitch(Π)
  step := step + 1
}
-}
centralLoop :: KEnv -> Procs -> PChInsns -> [Loop] -> Stmt
centralLoop kenv ps atomicOps loops =
  let l = loopMonitors loops
      k = channelBounds kenv
      pc = counterInvariants ps
      enabled = enabledExp kenv ps
      m = channelMonitors atomicOps loops
   in While
        enabled
        (k ++ pc ++ l ++ m)
        [Any]
        ( Block
            [ scheduleSwitch ps,
              Assign [("step", Plus ("step" @) (1 #))]
            ]
        )

{- Constructs an initial assignment for all program counters.
Depends on: Π

Produces:
var pc(π)₁, ..., pc(π)ₙ = 0, ..., 0
-}
counterDef :: Procs -> Stmt
counterDef procs =
  if M.size procs > 0
    then
      let def pid = ((pid <|), (0 #))
       in Assign . L.map def . M.keys $ procs
    else Assert (True ?)

{- Constructs an initial assignment for all loop variables.
Depends on: loop(P)

Produces:
var x₁, ..., xₙ = lo₁, ..., loₙ
-}
loopVarDef :: [Loop] -> Stmt
loopVarDef = \case
  [] -> Assert (True ?)
  ls ->
    let def (Loop {var, lower}) = (var, Nothing, lower)
     in VarDef False . L.map def $ ls

{- Constructs an initial assignment for all channel variables.
Depends on: κ

Produces:
var c₁, ..., cₙ = 0, ..., 0
-}
chanDef :: KEnv -> Stmt
chanDef kenv =
  if M.size kenv > 0
    then
      let def c = (c, Nothing, (0 #))
       in VarDef False . L.map def . M.keys $ kenv
    else Assert (True ?)

{- Construcs the "isSchedule(S)" precondition.
-}
isSchedule :: Exp
isSchedule = Call "isSchedule" [("S" @)]

{- Constructs the main program encoding.
Depends on: κ, Π, nonloop(P), loop(P), fv(P)

Produces:
method Program(S : nat -> nat, ∀ x ∈ fv(P). x : int)
returns (∀ (π, ϕ) ∈ Π. pc(π) : int)

requires asyncPreconditions(κ)
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
progEncoding :: S.Set String -> KEnv -> Procs -> PChInsns -> [Loop] -> Method
progEncoding fvs kenv ps atomicOps loops =
  Method
    { returns = (L.map ((,TInt) . (<|)) . M.keys) ps,
      methodHoare =
        HoareWrap
          { name = "Program",
            params = ("S", Arrow TNat TNat) : (L.map (,TInt) . S.toList) fvs,
            ensures = postconditions ps,
            decreases = [Any],
            requires =
              isSchedule
                : asyncPreconditions kenv
                ++ preconditions kenv atomicOps loops
          },
      methodBody =
        Block
          [ counterDef ps,
            chanDef kenv,
            loopVarDef loops,
            VarDef False [("step", Nothing, (0 #))],
            centralLoop kenv ps atomicOps loops
          ]
    }

{- Constructs the complete program specification, by emitting
all the necessary functions, and the program encoding.
-}
wholeEncoding :: S.Set String -> KEnv -> Procs -> PChInsns -> [Loop] -> Program
wholeEncoding fvs kenv ps atomicOps loops =
  Program
    [ FDecl iterationsFunc,
      FDecl (isScheduleFunc ps),
      MDecl (progEncoding fvs kenv ps atomicOps loops)
    ]