module Backend.Utilities where

import Data.List qualified as L
import Data.Map qualified as M
import Data.Set qualified as S

import Backend.Ast
import Utilities.Collection

-- | An alias for variable names (as strings) to clarify type definitions
type 𝑋 = String

-- | The type of type environments
type 𝛤 = 𝑋 ↦ Type

{- | Composes lists of expressions under arbitrary binary
constructors, ⊕, with a user-provided zero element 𝟎.
Depends on: E = [e₁, ..., eₙ]

Produces:
  if |E| = 0. 𝟎
  if |E| > 0. ⨁ eᵢ ≠ 𝟎. eᵢ
-}
(⨁) :: Exp -> (Exp -> Exp -> Exp) -> [Exp] -> Exp
(⨁) zero cons = \case
  [] -> zero
  e : es ->
    if e == zero
      then (zero ⨁ cons) es
      else cons e ((zero ⨁ cons) es)

-- | Compose lists of expressions under syntactic conjunction (&&).
(...⋀) :: [Exp] -> Exp
(...⋀) = (True ?) ⨁ (:&&)

-- | Compose lists of expressions under syntactic disjunction (||).
(...⋁) :: [Exp] -> Exp
(...⋁) = (False ?) ⨁ (:||)

-- | Compose lists of expressions under binary arithmetic.
arithmCompose :: (Exp -> Exp -> Exp) -> [Exp] -> Exp
arithmCompose = (⨁) (0 #)

-- | Compose lists of expressions under syntactic addition.
(...+) :: [Exp] -> Exp
(...+) = arithmCompose (:+)

-- | Compose lists of expressions under syntactic subtraction.
(...-) :: [Exp] -> Exp
(...-) = arithmCompose (:-)

-- | Construct the syntactical expression equivalent of an integer.
(#) :: Int -> Exp
(#) = ECon . CNum

-- | Construct the syntactical expression equivalent of a boolean.
(?) :: Bool -> Exp
(?) True = ECon CTrue
(?) False = ECon CFalse

-- | Construct the syntactical expression equivalent of a variable with the name as the given string.
(@) :: String -> Exp
(@) = EVar

-- | Get the set of free variables in an expression.
eFVs :: Exp -> S.Set String
eFVs =
  let quantifier xs e =
        let xs' = L.map fst xs
         in S.difference (eFVs e) (S.fromList xs')
      variadic = S.unions . L.map eFVs
      bin e1 e2 = S.union (eFVs e1) (eFVs e2)
      patternNames = \case
        PTuple ps -> S.unions $ L.map patternNames ps
        PAdt _ ps -> S.unions $ L.map patternNames ps
        PVar x -> S.singleton x
        _ -> S.empty
   in \case
    Match e cs ->
      let kase (p, e') = S.difference (eFVs e') (patternNames p)
       in S.unions $ eFVs e : L.map kase cs
    IfElse e1 e2 e3 -> S.unions $ L.map eFVs [e1, e2, e3]
    Exists xs e -> quantifier xs e
    Forall xs e -> quantifier xs e
    In e1 e2 -> bin e1 e2
    ESet es -> variadic es
    e1 :<==> e2 -> bin e1 e2
    e1 :==> e2 -> bin e1 e2
    e1 :&& e2 -> bin e1 e2
    e1 :|| e2 -> bin e1 e2
    e1 :== e2 -> bin e1 e2
    e1 :!= e2 -> bin e1 e2
    e1 :>= e2 -> bin e1 e2
    e1 :> e2 -> bin e1 e2
    e1 :<= e2 -> bin e1 e2
    e1 :< e2 -> bin e1 e2
    e1 :+ e2 -> bin e1 e2
    e1 :- e2 -> bin e1 e2
    e1 :* e2 -> bin e1 e2
    e1 :/ e2 -> bin e1 e2
    e1 :% e2 -> bin e1 e2
    Not e -> eFVs e
    ETuple es -> variadic es
    EVar x -> S.singleton x
    Call _ es -> variadic es
    _ -> S.empty

-- | Get the type of a Dafny expression, relative to a type environment.
typeOfExp :: 𝛤 -> Exp -> Type
typeOfExp 𝛾 =
  let typeOf = typeOfExp 𝛾
  in \case
    Match _ [] -> TBad
    Match _ ((_, e) : _) -> typeOf e
    IfElse _ e _ -> typeOf e
    Exists {} -> TBool
    Forall {} -> TBool
    In {} -> TBool
    ESet [] -> TBad
    ESet (e : _) -> TSet (typeOf e)
    _ :<==> _ -> TBool
    _ :==> _ -> TBool
    _ :&& _ -> TBool
    _ :|| _ -> TBool
    _ :== _ -> TBool
    _ :!= _ -> TBool
    _ :>= _ -> TBool
    _ :> _ -> TBool
    _ :<= _ -> TBool
    _ :< _ -> TBool
    _ :+ _ -> TInt
    _ :- _ -> TInt
    _ :* _ -> TInt
    _ :/ _ -> TInt
    _ :% _ -> TInt
    Not _ -> TBool
    ETuple es -> Tuple $ L.map typeOf es
    Any -> TBad
    EVar x -> 𝛾 M.! x
    ECon c -> typeOfConst c
    Call "iter" _ -> TInt
    -- Calls to arbitrary functions not yet supported
    Call {} -> TBad

typeOfConst :: Const -> Type
typeOfConst = \case
  CTrue -> TBool
  CFalse -> TBool
  CNum _ -> TInt
