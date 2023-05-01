module Utilities.General (results, foldMonad, binaryCons, unaryCons) where

results :: Monad m => [m a] -> m [a]
results [] = return []
results (r : rs) = do
  rv <- r
  rs' <- results rs
  return (rv : rs')

foldMonad :: (Foldable t1, Monad m) => (t2 -> m t3) -> t4 -> (t4 -> t3 -> t4) -> t1 t2 -> m t4
foldMonad process start combine =
  Prelude.foldl
    ( \ms e -> do
        s1 <- ms
        s2 <- process e
        return $ combine s1 s2
    )
    (return start)

binaryCons :: Monad m => (a -> m b) -> (b -> b -> c) -> a -> a -> m c
binaryCons p cons e1 e2 = do
  e1' <- p e1
  e2' <- p e2
  return (cons e1' e2')

unaryCons :: Monad m => (a -> m b) -> (b -> c) -> a -> m c
unaryCons p cons e = do
  e' <- p e
  return (cons e')