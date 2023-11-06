module Main where

import IR.GetAst qualified as I
import IR.Simplifier (simplify)
import Pipeline.Translation.Workflow
import Pipeline.Verification.Runner (verify)
import Promela.GetAst qualified as P
import System.Environment
import Utilities.Args
import Utilities.Err

main :: IO ()
main = do
  args <- getArgs
  filePath <- case getFilePath args of
    Ok filePath -> return filePath
    Bad _ -> ioError $ userError "Give me a Promela/VIRGo file."
  source <- readFile filePath
  let ir =
        if hasIRFlag args
          then I.getAst source
          else
            (do
                prom <- P.getAst source
                promelaToIR prom)
  p <- case ir of
    Ok ir' -> do
      let ir'' = simplify ir'
      putStrLn "\n"
      putStrLn "VIRGo translation:"
      print ir''
      putStrLn "\n"
      return ir''
    Bad msg -> ioError $ userError $ "VIRGo translation failed: " ++ msg
  verify args filePath p
