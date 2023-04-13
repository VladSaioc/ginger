module Utilities.General (results, binaryCons, unaryCons) where

results :: Monad m => Eq a => [m a] -> m [a]
results [] = return []
results (r : rs) = do
  rv <- r
  rs' <- results rs
  return (rv : rs')

binaryCons :: Monad m => (a -> m b) -> (b -> b -> c) -> a -> a -> m c
binaryCons p cons e1 e2 = do
  e1' <- p e1
  e2' <- p e2
  return (cons e1' e2')

unaryCons :: Monad m => (a -> m b) -> (b -> c) -> a -> m c
unaryCons p cons e = do
  e' <- p e
  return (cons e')