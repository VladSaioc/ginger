module Utilities.Position where

import Control.Monad (MonadPlus(..), liftM)
import Control.Applicative (Applicative(..), Alternative(..))

epos = -1

data Pos a = Pos Int a deriving (Read, Eq, Ord)

instance Monad Pos where
  return      = pure
  Pos l n  >>= f = case (l, f n) of
    (epos, Pos l' n') -> Pos l' n'
    (l, Pos epos n') -> Pos l n'
    (l, Pos _ n') -> Pos l n'

instance Applicative Pos where
  pure n = Pos epos n
  (Pos _ n) <*> o  = liftM n o

instance Functor Pos where
  fmap = liftM

instance Show a => Show (Pos a) where
  show (Pos l n) = "Line " ++ show l ++ ": " ++ show n ++ "\n"

(@) :: Int -> a -> Pos a
(@) p = Pos p

(@>) :: Pos a -> (a -> b) -> Pos b
(@>) (Pos l a) f = Pos l (f a)