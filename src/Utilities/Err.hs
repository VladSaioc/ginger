module Utilities.Err where

import Control.Applicative (Alternative (..), Applicative (..))
import Control.Monad (MonadPlus (..), liftM)

data Err a = Ok a | Bad String
  deriving (Read, Show, Eq, Ord)

instance Monad Err where
  return = pure
  Ok a >>= f = f a
  Bad s >>= _ = Bad s

instance Applicative Err where
  pure = Ok
  (Bad s) <*> _ = Bad s
  (Ok f) <*> o = liftM f o

instance Functor Err where
  fmap = liftM

instance MonadPlus Err where
  mzero = Bad "Error message"
  mplus (Bad _) y = y
  mplus x _ = x

instance Alternative Err where
  empty = mzero
  (<|>) = mplus

multiGuard :: [(Bool, String)] -> Err ()
multiGuard = \case
  [] -> return ()
  (g, msg) : guards ->
    if not g
      then multiGuard guards
      else Bad msg

isErr :: Err a -> Bool
isErr = \case
  Ok _ -> True
  Bad _ -> False