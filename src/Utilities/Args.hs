module Utilities.Args where

import Data.List (isPrefixOf)
import Utilities.Err

_DIST :: String
_DIST = "output"

hasIRFlag :: [String] -> Bool
hasIRFlag args = case args of
  [] -> False
  "-ir" : _ -> True
  _ : as -> hasIRFlag as

getFilePath :: [String] -> Err String
getFilePath = \case
  [] -> Bad "No file provided."
  [a] ->
    if "-" `isPrefixOf` a
      then Bad "No file provided."
      else return a
  _ : as -> getFilePath as

getResultDir :: [String] -> String
getResultDir = \case
  [] -> _DIST
  [_] -> _DIST
  "-output-dir" : a : _ -> a
  _ : as -> getResultDir as