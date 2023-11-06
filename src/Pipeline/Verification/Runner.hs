module Pipeline.Verification.Runner where

import Control.Monad (unless)
import Data.List (isInfixOf)
import Data.Text (pack, replace, unpack)
import GHC.IO.Exception (ExitCode(ExitSuccess, ExitFailure))
import System.Directory (createDirectory, doesDirectoryExist)
import System.Process (readProcessWithExitCode)
import System.Console.ANSI

import IR.Ast
import IR.Utilities (interesting)
import Pipeline.Verification.Dafny
import Pipeline.Verification.Oracle
import Pipeline.IRTranslation.Workflow (irToBackend)
import Utilities.Args (hasColor, getResultDir, getDafnyPath, skipVerification)
import Utilities.Err

-- | Print a colored message, if the `-color` flag is set to true.
printColoredMsg :: [String] -> Color -> String -> IO ()
printColoredMsg args c msg =
  if hasColor args
  then do
    setSGR [SetColor Foreground Vivid c]
    putStr msg
    setSGR [Reset]
  else putStr msg

-- | Make a directory. Acts as no-op if directory already exists.
mkdir :: [Char] -> IO ()
mkdir dir = do
  putStrLn ("Checking directory " ++ dir ++ "...")
  dirExists <- doesDirectoryExist dir
  unless dirExists $ createDirectory dir

-- | Verify a VIRGo program
verify :: [String] -> String -> 𝑃 -> IO ()
verify args sourceFile p = do
  let dafnyBin = getDafnyPath args
  let outputDir = getResultDir args
  let colorPrint = printColoredMsg args
  let parseFileName = unpack . replace (pack "/") (pack "__") . pack
  let oracles = [
          trivial,
          balancedFlowWP,
          balancedFlow
        ]
  _ <- if interesting p
    then return True
    else ioError $ userError "Example is not interesting."
  encoding <- case irToBackend p of
    Ok encoding -> do
      return encoding
    Bad msg -> ioError $ userError $ "Back-end translation failed: " ++ msg
  putStrLn "Successfully encoded VIRGo program to back-end."
  mkdir outputDir
  let printDafny oracle = do
        let outputFile = outputDir ++ "/" ++ parseFileName sourceFile ++ "-" ++ shortName oracle ++ ".dfy"
        writeFile outputFile (show $ encodingToDafny oracle encoding)
        putStrLn $ "Dafny encoding found at: " ++ outputFile
        return outputFile
  let verifyOne oracle = do
        putStrLn $ "\nAttempting to run oracle " ++ oname oracle
        outputFile <- printDafny oracle
        (exitCode, successMsg, errMsg) <- readProcessWithExitCode dafnyBin [outputFile] ""
        case exitCode of
          ExitFailure _ -> do
            colorPrint Red $ "Failed verification with Oracle " ++ oname oracle ++ "\n"
            return False
          ExitSuccess ->
            if "Dafny program verifier finished with 1 verified, 0 errors" `isInfixOf` successMsg
              then do
                colorPrint Green "Verification successful!\n"
                putStrLn $ successMessage oracle encoding
                return True
              else do
                putStrLn $ unlines [
                    "Verification did not produce expected message. Actual output:",
                    successMsg,
                    "Error stream:",
                    errMsg
                  ]
                return False
  let iterateOracles = \case
        [] -> ioError $ userError "Verification failed for all oracles."
        oracle : oracles' -> do
          verificationResult <- verifyOne oracle
          if verificationResult then return () else iterateOracles oracles'
  if skipVerification args
    then do
      putStrLn "Emitting back-end without verification under the `WP Balanced Flow` strategy."
      _ <- printDafny balancedFlowWP
      return ()
    else iterateOracles oracles
