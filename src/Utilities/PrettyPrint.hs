module Utilities.PrettyPrint where

class PrettyPrint a where
  prettyPrint :: Int -> a -> String

indent :: Int -> String
indent n = concat (replicate n "  ")