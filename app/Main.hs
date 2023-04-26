module Main where

import Backend.Optimizer (optimize)
import Control.Monad (unless)
import Data.Text (pack, replace, unpack)
import IR.GetAst qualified as I
import Pipeline.IRTranslation.Workflow
import Promela.Ast (Spec (Spec))
import Promela.GetAst
import System.Directory (createDirectory, doesDirectoryExist)
import System.Environment
import Utilities.Err
import Utilities.PrettyPrint (PrettyPrint (prettyPrint))

_DIST :: String
_DIST = "output"

mkdir :: [Char] -> IO ()
mkdir dir = do
  putStrLn ("Checking directory " ++ dir ++ "...")
  dirExists <- doesDirectoryExist _DIST
  unless dirExists $ createDirectory _DIST

main :: IO ()
main = do
  args <- getArgs
  let parseFileName = unpack . replace (pack "/") (pack "#") . pack
  let workflow file source =
        let ast = getAst source
         in do
              case ast of
                Ok (Spec _) -> putStr "Ok"
                Bad err -> putStr ("Error in parsing " ++ file ++ ": " ++ err)

  case args of
    ("ir" : filePath : _) -> do
      let fileName = parseFileName filePath ++ ".dfy"
      source <- readFile filePath
      let result = do
            ir <- I.getAst source
            prog <- irToBackend ir
            return $ optimize prog
      _ <-
        ( case result of
            Ok prog -> do
              putStrLn "Succesfully generated and optimized back-end."
              mkdir _DIST
              writeFile (_DIST ++ "/" ++ fileName) (prettyPrint 0 prog)
              return ()
            Bad err -> putStrLn ("ERROR: " ++ err)
          )
      putStr "Done.\n"
    (fileName : _) -> do
      source <- readFile fileName
      workflow fileName source
    _ -> do
      putStr "Give me a Promela file."