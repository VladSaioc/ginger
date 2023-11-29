module Utilities.Err where

import Control.Applicative (Alternative (..))
import Control.Monad (MonadPlus (..), liftM)
import Control.Monad.IO.Class
import Data.Map qualified as M
import Data.Maybe qualified as Mb

data Err a = Ok a | Bad String
  deriving (Read, Show, Eq, Ord)

instance Monad Err where
  return = pure
  Ok a >>= f = f a
  Bad s >>= _ = Bad s

instance MonadFail Err where
  fail = Bad

instance Applicative Err where
  pure = Ok
  (Bad s) <*> _ = Bad s
  (Ok f) <*> o = fmap f o

instance Functor Err where
  fmap = liftM

instance MonadPlus Err where
  mzero = Bad "Error message"
  mplus (Bad s) _ = Bad s
  mplus _ (Bad s) = Bad s
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

mlookup :: (MonadFail m, Ord p) => String -> p -> M.Map p a -> m a
mlookup errmsg k m = do
  let v = M.lookup k m
  Mb.maybe (fail errmsg) pure v

newtype ErrT m a = ErrT {runErrT :: m (Err a)}

instance Monad m => Functor (ErrT m) where
  fmap f (ErrT o) = ErrT $ do
    x <- o
    case x of
      Bad msg -> return $ Bad msg
      Ok v -> return $ return $ f v

instance Monad m => Applicative (ErrT m) where
  pure = ErrT . return . return
  ErrT ff <*> ErrT fa = ErrT $ do
    f <- ff
    case f of
      Bad msg -> do
        return $ Bad msg
      Ok f' -> do
        a <- fa
        case a of
          Bad msg -> return $ Bad msg
          Ok v -> return $ return $ f' v

instance Monad m => Monad (ErrT m) where
  return = pure
  x >>= f = ErrT $ do
    x' <- runErrT x
    case x' of
      Bad msg -> do
        return $ Bad msg
      Ok v -> runErrT $ f v

newtype ErrIO a = ErrIO {runErrIO :: IO (Err a)}

instance MonadIO ErrIO where
  liftIO a = ErrIO (return <$> a)

instance Functor ErrIO where
  fmap f (ErrIO o) = ErrIO $ do
    x <- o
    case x of
      Bad msg -> return $ Bad msg
      Ok v -> return $ return $ f v

instance Applicative ErrIO where
  pure = ErrIO . return . return
  ErrIO ff <*> ErrIO fa = ErrIO $ do
    f <- ff
    case f of
      Bad msg -> do
        return $ Bad msg
      Ok f' -> do
        a <- fa
        case a of
          Bad msg -> do
            putStrLn msg
            return $ Bad msg
          Ok v -> return $ return $ f' v

instance Monad ErrIO where
  return = pure
  x >>= f = ErrIO $ do
    x' <- runErrIO x
    case x' of
      Bad msg -> do
        putStrLn msg
        return $ Bad msg
      Ok v -> runErrIO $ f v
