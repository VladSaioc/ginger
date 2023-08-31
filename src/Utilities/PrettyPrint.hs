module Utilities.PrettyPrint where

import Data.List (intercalate)

-- | PrettyPrint is geared towards indented unparsing of syntax values.
class PrettyPrint a where
  prettyPrint :: Int -> a -> String

-- | Intercalates multiple strings with newline.
multiline :: [[Char]] -> [Char]
multiline = intercalate "\n"

-- | Prepends with an indentation of n double spaces.
indent :: Int -> String -> String
indent n = (concat (replicate n "  ") ++)
