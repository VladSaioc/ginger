module Utilities.PrettyPrint where

import Data.List (intercalate)

class PrettyPrint a where
  prettyPrint :: Int -> a -> String

multiline :: [[Char]] -> [Char]
multiline = intercalate "\n"

indent :: Int -> String
indent n = concat (replicate n "  ")