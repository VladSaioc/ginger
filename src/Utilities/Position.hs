module Utilities.Position where

import Control.Monad (liftM)
import Utilities.Err
import Utilities.PrettyPrint

-- | Pattern alias to match for no positional information.
pattern NoPos :: (Eq a, Num a) => a
pattern NoPos = -1

-- | Provides any construct with some positional information.
data Pos a = Pos Int a deriving (Read, Eq, Ord)

-- | Pretty print datum without positional information.
instance PrettyPrint a => PrettyPrint (Pos a) where
  prettyPrint n (Pos _ a) = prettyPrint n a

-- | Having positional information encoded as a monad allows
-- the use of 'do' notation. Furthermore, when using
-- monadic composition for two data where one does not have positional
-- information, it allows positional information to be propagated to the next
-- datum.
instance Monad Pos where
  return = pure
  -- | When composing elements without position, favor the position of elements with position.
  -- When both elements have positions, favor the left-most one.
  Pos l n >>= f = case (l, f n) of
    (NoPos, Pos l' n') -> Pos l' n'
    (l', Pos NoPos n') -> Pos l' n'
    (l', Pos _ n') -> Pos l' n'

instance Applicative Pos where
  pure = Pos NoPos
  (Pos p n) <*> (Pos NoPos l) = fmap n (Pos p l)
  (Pos _ n) <*> (Pos p l) = fmap n (Pos p l)

instance Functor Pos where
  fmap = liftM

instance Show a => Show (Pos a) where
  show (Pos l n) = "Line " ++ show l ++ ": " ++ show n ++ "\n"

(@) :: Int -> a -> Pos a
(@) = Pos

(@>) :: Pos a -> (a -> b) -> Pos b
(@>) (Pos l a) f = Pos l (f a)

posErrMsg :: Int -> String -> String
posErrMsg p msg = ":" ++ show p ++ ": " ++ msg

posErr :: Int -> String -> Err a
posErr p msg = Bad $ posErrMsg p msg

(@^) :: Pos a -> a
(@^) (Pos _ a) = a
