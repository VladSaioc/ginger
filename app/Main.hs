module Main where

import Control.Monad (unless, when)
import Data.Text (pack, replace, unpack)
import IR.GetAst qualified as I
import IR.Utilities (interesting)
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
      let file = parseFileName filePath ++ ".dfy"
      let out = getResultDir args
      source <- readFile filePath
      let ir =
            if hasIRFlag args
              then I.getAst source
              else
                ( do
                    prom <- P.getAst source
                    promelaToIR prom
                )
      let result =
            ( do
                ir' <- ir
                prog <- irToBackend ir'
                return (prog, interesting ir')
            )
      _ <- case ir of
        Ok ir' -> do
          putStrLn "\n"
          putStrLn "Intermediate representation:"
          print ir'
          putStrLn "\n"
        _ -> return ()
      case result of
        Ok (prog, interestingProg) ->
          ( do
              putStrLn "Succesfully generated and simplified back-end."
              mkdir out
              when interestingProg (do
                writeFile (out ++ "/" ++ file) (prettyPrint 0 prog)
                putStrLn $ "Output found at: " ++ out ++ "/" ++ file)
              unless interestingProg $ putStrLn "Example is not interesting."
          )
        Bad err -> ioError $ userError ("ERROR: " ++ filePath ++ err)
    Bad _ -> ioError $ userError "Give me a Promela/Go IR file."
