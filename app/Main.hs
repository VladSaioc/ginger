module Main where

import Control.Monad (unless)
import System.Environment

import Go.Profiler (profileProgram)
import IR.GetAst qualified as I
import IR.Profiler (profileVirgo)
import IR.Simplifier (simplify)
import IR.Utilities (interesting)
import Pipeline.Translation.Workflow (promelaToGo, goToIR)
import Pipeline.Verification.Runner (verify)
import Promela.GetAst qualified as P
import Utilities.Args
import Utilities.Err

main :: IO ()
main = do
  args <- getArgs
  filePath <- case getFilePath args of
    Ok filePath -> return filePath
    Bad _ -> ioError $ userError "Give me a Promela/VIRGo file."
  source <- readFile filePath
  ir <-
        if hasIRFlag args
          then return $ I.getAst source
          else do
              let mg = do
                    prom <- P.getAst source
                    promelaToGo prom
              case mg of
                Ok g -> do
                  let msg = profileProgram g
                  putStrLn $ unlines ["Profiling Go program parametricity:", msg]
                  return (goToIR g)
                Bad msg -> return (Bad msg)
  p <- case ir of
    Ok ir' -> do
      let ir'' = simplify ir'
      unless (interesting ir'') $ ioError $ userError "Program is not interesting."
      putStrLn "\n"
      putStrLn "VIRGo translation:"
      print ir''
      putStrLn "\n"
      putStrLn $ unwords ["VirGo program parametricity:", profileVirgo ir']
      return ir''
    Bad msg -> ioError $ userError $ "VIRGo translation failed: " ++ msg
  verify args filePath p
