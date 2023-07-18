module Utilities.Args where

import Data.List (intercalate, isPrefixOf)
import Data.List.Split (splitOn)
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
getResultDir args =
  let dir = case getFilePath args of
        Ok filePath ->
          let isAbs = "/" `isPrefixOf` filePath
              parts = splitOn "/" filePath
              dirPath = intercalate "/" (take (length parts - 1) parts)
           in if isAbs
                then dirPath
                else "./" ++ dirPath
        Bad _ -> _DIST
   in case args of
        [] -> dir
        [_] -> dir
        "-output-dir" : a : _ ->
          if "/" `isPrefixOf` a
            then a
            else "./" ++ a
        _ : as -> getResultDir as