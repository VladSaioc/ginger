module Main where

import System.IO
import System.Environment
import System.Directory

import Control.Monad
import Text.Pretty.Simple (pPrint)
import Data.List.Split (splitOn)

import Promela.ParPromela
import Promela.ErrM

_DIST = "dist"

includes _ [] = False
includes v (x:xs) = (v == x) || includes v xs

getAst s =
  let
    tokens = myLexer s
    ast = pSpec tokens
  in ast

main :: IO ()
main = do
  args <- getArgs
  let
    workflow (file : opts) source =
      let ast = getAst source
      in do
        case ast of
          Ok _ -> putStr "Ok"
          Bad err -> putStr err

  case args of
    (fileName : opts) -> do
      source <- readFile fileName
      workflow (fileName : opts) source
    _ -> do
      putStr "Give me a Promela file."