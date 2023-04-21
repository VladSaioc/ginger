module Backend.Utilities where

import Backend.Ast

{- Composes lists of expressions under arbitrary binary
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

(./\.) :: [Exp] -> Exp
(./\.) = ECon CTrue ⨁ And

(.\/.) :: [Exp] -> Exp
(.\/.) = ECon CFalse ⨁ Or

arithmCompose :: (Exp -> Exp -> Exp) -> [Exp] -> Exp
arithmCompose = (⨁) (0 #)

(.+.) :: [Exp] -> Exp
(.+.) = arithmCompose Plus

(.-.) :: [Exp] -> Exp
(.-.) = arithmCompose Minus

(#) :: Int -> Exp
(#) = ECon . CNum

(@) :: String -> Exp
(@) = EVar