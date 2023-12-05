module IR.Profiler (profileVirgo) where

import Data.List qualified as L
import Data.Monoid

import IR.Ast

data Parametricity = ChLooping | WgLooping | WgAdd | Capacity | PathCondition
  deriving (Read)

instance Show Parametricity where
  show = \case
    ChLooping -> "chan-loop"
    WgLooping -> "wg-loop"
    WgAdd -> "wg-add"
    Capacity -> "capacity"
    PathCondition -> "path condition"

profileVirgo :: 𝑃 -> String
profileVirgo p =
  let parametricity = getParametricity p
   in if null parametricity then "not parametric."
      else L.intercalate "; " (map show parametricity)

getParametricity :: 𝑃 -> [Parametricity]
getParametricity p =
  let chlooping =  ([ChLooping | Any True == chLoopParametric p])
      wglooping =  ([WgLooping | Any True == wgLoopParametric p])
      add =  ([WgAdd | Any True == wgAddParametric p])
      cap = ([Capacity | Any True == capParametric p])
    in chlooping ++ wglooping ++ add ++ cap

traverseStmt :: Monoid a => Monoid b => (a -> 𝑆 -> a) -> (a -> 𝑆 -> b) -> a -> 𝑆 -> b
traverseStmt makecontext makeresult ctx s =
  let tr = traverseStmt makecontext makeresult (makecontext ctx s)
      res = makeresult ctx
  in case s of
    Seq s1 s2 -> res s <> tr s1 <> tr s2
    Go s' -> res s <> tr s'
    For _ _ _ os ->
      let mkres c = makeresult c . Atomic
       in res s <> mconcat (map (traverseOp mkres (makecontext ctx s)) os)
    If _ ss1 ss2 -> res s <> tr ss1 <> tr ss2
    _ -> res s

traverseOp :: (a -> Op -> b) -> a -> Op -> b
traverseOp makeresult = makeresult

chLoopParametric :: 𝑃 -> Any
chLoopParametric (𝑃 _ s) =
  let makecontext looping = \case
        For _ e1 e2 _ -> Any (parametricExp e1) <> Any (parametricExp e2)
        _ -> looping
      makeresult looping = \case
        Atomic (Send _) -> looping
        Atomic (Recv _) -> looping
        _ -> Any False
    in traverseStmt makecontext makeresult (Any False) s

wgLoopParametric :: 𝑃 -> Any
wgLoopParametric (𝑃 _ s) =
  let makecontext looping = \case
        For _ e1 e2 _ -> Any (parametricExp e1) <> Any (parametricExp e2)
        _ -> looping
      makeresult looping = \case
        Atomic (Add _ _) -> looping
        Atomic (Wait _) -> looping
        _ -> Any False
    in traverseStmt makecontext makeresult (Any False) s

wgAddParametric :: 𝑃 -> Any
wgAddParametric (𝑃 _ s) =
  let makecontext _ _ = Any False
      makeresult _ = \case
        Atomic (Add _ e) -> Any $ parametricExp e
        _ -> Any False
    in traverseStmt makecontext makeresult (Any False) s

capParametric :: 𝑃 -> Any
capParametric (𝑃 cs _) =
  let checkCap = \case
        Chan _ e -> Any $ parametricExp e
        _ -> Any False
   in mconcat (map checkCap cs)

parametricExp :: 𝐸 -> Bool
parametricExp =
  let bin e1 e2 = parametricExp e1 || parametricExp e2
  in \case
    Const _ -> False
    BFalse -> False
    BTrue -> False
    e1 :& e2 -> bin e1 e2
    e1 :| e2 -> bin e1 e2
    e1 :== e2 -> bin e1 e2
    e1 :!= e2 -> bin e1 e2
    e1 :< e2 -> bin e1 e2
    e1 :<= e2 -> bin e1 e2
    e1 :> e2 -> bin e1 e2
    e1 :>= e2 -> bin e1 e2
    Not e -> parametricExp e
    e1 :+ e2 -> bin e1 e2
    e1 :- e2 -> bin e1 e2
    e1 :* e2 -> bin e1 e2
    e1 :/ e2 -> bin e1 e2
    Var _ -> True
