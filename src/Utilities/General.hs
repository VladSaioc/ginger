module Utilities.General (results, binaryCons, unaryCons) where

import Control.Monad

import Utilities.Err
import Utilities.Position

errMsg :: String -> Integer -> String -> Err alpha
errMsg file row msg = Bad (file ++ ":" ++ show row ++ ": " ++ msg)

results :: Monad m => Eq alpha => [m alpha] -> m [alpha]
results [] = return []
results (r:rs) = do
  rv <- r
  rs <- results rs
  return (rv : rs)

binaryCons :: Monad m => (a -> m b) -> (b -> b -> c) -> a -> a -> m c
binaryCons p cons e1 e2 = do
  e1 <- p e1
  e2 <- p e2
  return (cons e1 e2)


unaryCons :: Monad m => (a -> m b) -> (b -> c) -> a -> m c
unaryCons p cons e = do
  e <- p e
  return (cons e)