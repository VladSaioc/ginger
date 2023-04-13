module Utilities.Position where

import Control.Monad (liftM)

pattern NoPos :: (Eq a, Num a) => a
pattern NoPos = -1

data Pos a = Pos Int a deriving (Read, Eq, Ord)

instance Monad Pos where
  return = pure
  Pos l n >>= f = case (l, f n) of
    (NoPos, Pos l' n') -> Pos l' n'
    (l', Pos NoPos n') -> Pos l' n'
    (l', Pos _ n') -> Pos l' n'

instance Applicative Pos where
  pure = Pos NoPos
  (Pos _ n) <*> o = fmap n o

instance Functor Pos where
  fmap = liftM

instance Show a => Show (Pos a) where
  show (Pos l n) = "Line " ++ show l ++ ": " ++ show n ++ "\n"

(@) :: Int -> a -> Pos a
(@) = Pos

(@>) :: Pos a -> (a -> b) -> Pos b
(@>) (Pos l a) f = Pos l (f a)