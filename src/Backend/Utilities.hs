module Backend.Utilities where

import Backend.Ast

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
