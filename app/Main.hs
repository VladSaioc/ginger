module Main where

import Backend.Optimizer (optimize)
import Control.Monad (unless)
import Data.Text (pack, replace, unpack)
import IR.GetAst qualified as I
import Pipeline.IRTranslation.Workflow
import Pipeline.Translation.Workflow
import Promela.GetAst qualified as P
import System.Directory (createDirectory, doesDirectoryExist)
import System.Environment
import Utilities.Args
import Utilities.Err
import Utilities.PrettyPrint (PrettyPrint (prettyPrint))

mkdir :: [Char] -> IO ()
mkdir dir = do
  putStrLn ("Checking directory " ++ dir ++ "...")
  dirExists <- doesDirectoryExist dir
  unless dirExists $ createDirectory dir

main :: IO ()
main = do
  args <- getArgs
  let parseFileName = unpack . replace (pack "/") (pack "#") . pack
  let workflow out file ir =
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
                  print ir'
                  putStrLn "\n"
                _ -> return ()
              case result of
                Ok prog ->
                  ( do
                      putStrLn "Succesfully generated and optimized back-end."
                      mkdir out
                      writeFile (out ++ "/" ++ file) (prettyPrint 0 prog)
                      return ()
                  )
                Bad err -> putStrLn ("ERROR: " ++ err)
  case getFilePath args of
    Ok filePath -> do
      let outputFileName = parseFileName filePath ++ ".dfy"
      let outputDir = getResultDir args
      source <- readFile filePath
      let ir =
            if hasIRFlag args
              then I.getAst source
              else
                ( do
                    prom <- P.getAst source
                    promelaToIR prom
                )
      _ <- workflow outputDir outputFileName ir
      putStr "Done.\n"
    Bad _ -> putStr "Give me a Promela file."