module Main where

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
  let parseFileName = unpack . replace (pack "/") (pack "__") . pack
  case getFilePath args of
    Ok filePath -> do
      let workflow out file ir =
            let result =
                  ( do
                      ir' <- ir
                      irToBackend ir'
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
                          putStrLn "Succesfully generated and simplified back-end."
                          mkdir out
                          writeFile (out ++ "/" ++ file) (prettyPrint 0 prog)
                          return ()
                      )
                    Bad err -> ioError $ userError ("ERROR: " ++ filePath ++ err)
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
      putStrLn $ "Output found at: " ++ outputDir ++ "/" ++ outputFileName
    Bad _ -> ioError $ userError "Give me a Promela/Go IR file."
