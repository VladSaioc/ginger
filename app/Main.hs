module Main where

import Backend.Optimizer (optimize)
import Control.Monad (unless)
import Data.Text (pack, replace, unpack)
import IR.GetAst qualified as I
import Pipeline.IRTranslation.Workflow
import Pipeline.Translation.Workflow
import Promela.Ast (Spec (Spec))
import Promela.GetAst qualified as P
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
  let workflow file ir =
        let result =
              ( do
                  ir' <- ir
                  prog <- irToBackend ir'
                  return $ optimize prog
              )
         in do
            _ <- case ir of
              Ok ir' -> do
                putStrLn "\n"
                putStrLn "Intermediate representation:"
                putStrLn (show ir')
                putStrLn "\n"
              _ -> return ()
            case result of
              Ok prog ->
                ( do
                    putStrLn "Succesfully generated and optimized back-end."
                    mkdir _DIST
                    writeFile (_DIST ++ "/" ++ file) (prettyPrint 0 prog)
                    return ()
                )
              Bad err -> putStrLn ("ERROR: " ++ err)
  case args of
    ("ir" : filePath : _) -> do
      let fileName = parseFileName filePath ++ ".dfy"
      source <- readFile filePath
      let ir = I.getAst source
      _ <- workflow fileName ir
      putStr "Done.\n"
    (filePath : _) -> do
      let fileName = parseFileName filePath ++ ".dfy"
      source <- readFile filePath
      let ir =
            ( do
                prom <- P.getAst source
                promelaToIR prom
            )
      workflow fileName ir
    _ -> do
      putStr "Give me a Promela file."