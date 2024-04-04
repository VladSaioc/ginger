module Backend.Utilities where

import Data.List qualified as L
import Data.Monoid qualified as Mo
import Data.Map qualified as M
import Data.Set qualified as S

import Backend.Ast
import Utilities.Collection
import Utilities.PrettyPrint

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
(⨁) zero' cons = \case
  [] -> zero'
  e : es ->
    if e == zero'
      then (zero' ⨁ cons) es
      else cons e ((zero' ⨁ cons) es)

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

-- | Check whether an expression contains any conditional statements
containsConditionals :: Exp -> Bool
containsConditionals e =
  let Mo.Any b = aggregateExpression (\case
        IfElse {} -> Mo.Any True
        Match {} -> Mo.Any True
        _ -> Mo.Any False) e
   in b

-- | Get the set of free variables in an expression.
aggregateExpression :: Monoid m => (Exp -> m) -> Exp -> m
aggregateExpression f e0 =
  let fold = aggregateExpression f
      variadic = foldl (<>) mempty . L.map fold
      bin e1 e2 = fold e1 <> fold e2
   in f e0 <> case e0 of
    Match e cs -> f e <> variadic (L.map snd cs)
    IfElse e1 e2 e3 -> variadic [e1, e2, e3]
    Exists _ e -> variadic [e]
    Forall _ e -> variadic [e]
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
    Not e -> variadic [e]
    ETuple es -> variadic es
    Call _ es -> variadic es
    _ -> mempty

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
    Neg _ -> TInt
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

propositionalPrintType :: Type -> String
propositionalPrintType = \case
  TBad -> error "!<Invalid type>!"
  TInt -> "ℤ"
  TNat -> "ℕ"
  TBool -> "𝔹"
  TSet t -> "℘(" ++ propositionalPrintType t ++ ")"
  TVar t -> t
  t1 :-> t2 -> propositionalPrintType t1 ++ "⟹" ++ propositionalPrintType t2
  Tuple ts -> L.intercalate " × " (map propositionalPrintType ts)

-- | Pretty prints a proposition.
propositionalPrintExp :: Exp -> String
propositionalPrintExp e =
  let pp = propositionalPrintExp
      quantifier q xs e' =
        let def (x, mt) =
              let t' = case mt of
                    Just t -> " ∈ " ++ propositionalPrintType t
                    Nothing -> ""
               in x ++ t'
            xs' = L.intercalate ", " $ map def xs
         in unwords [q, xs', ".", pp e']
      bin e1 op e2 = unwords [(<.|.>) pp e (Left e1), op, (<.|.>) pp e (Right e2)]
      un op e' = unwords [op ++ e <.> e']
  in case e of
    Any -> "_"
    ETuple es -> "(" ++ L.intercalate ", " (map pp es) ++ ")"
    EVar x -> x
    ECon c -> prettyPrint 0 c
    In e1 e2 -> bin e1 "∈" e2
    ESet es -> unwords ["{", L.intercalate ", " $ map pp es, "}"]
    Exists xs e' -> quantifier "∃" xs e'
    Forall xs e' -> quantifier "∀" xs e'
    e1 :<==> e2 -> bin e1 "⟺" e2
    e1 :==> e2 -> bin e1 "⟹" e2
    e1 :&& e2 -> bin e1 "∧" e2
    e1 :|| e2 -> bin e1 "∨" e2
    Not e' -> un "¬" e'
    Neg e -> un "-" e
    e1 :== e2 -> bin e1 "=" e2
    e1 :!= e2 -> bin e1 "≠" e2
    e1 :>= e2 -> bin e1 "≥" e2
    e1 :> e2 -> bin e1 ">" e2
    e1 :<= e2 -> bin e1 "≤" e2
    e1 :< e2 -> bin e1 "<" e2
    e1 :+ e2 -> bin e1 "+" e2
    e1 :- e2 -> bin e1 "-" e2
    e1 :* e2 -> bin e1 "⋅" e2
    e1 :/ e2 -> bin e1 "/" e2
    e1 :% e2 -> bin e1 "%" e2
    IfElse e1 e2 e3 -> unwords ["if", pp e1, "then", pp e2, "else", pp e3]
    Match e' cs ->
      let def (p, e'') = unwords ["case", prettyPrint 0 p, "=>", pp e'']
          cs' = map def cs
       in unwords ["match", pp e', "{\n"]
            ++ unlines cs'
            ++ ("\n" ++ "}")
    Call f es -> f ++ "(" ++ L.intercalate ", " (map pp es) ++ ")"
