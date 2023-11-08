module Pipeline.Verification.Runner (verify) where

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
import Pipeline.Verification.TermVerifier (unsatExpression)
import Utilities.Args (getResultDir, getDafnyPath, skipVerification)
import Utilities.Color
import Utilities.Err 
import Backend.Simplifier (eSimplify)
import Backend.Utilities (propositionalPrintExp)

-- | Make a directory. Acts as no-op if directory already exists.
mkdir :: [Char] -> IO ()
mkdir dir = do
  putStrLn ("Checking directory " ++ dir ++ "...")
  dirExists <- doesDirectoryExist dir
  unless dirExists $ createDirectory dir

-- | Verify a VIRGo program encoding using Dafny.
verify :: [String] -> String -> 𝑃 -> IO ()
verify args sourceFile p = do
  -- Get Dafny binary as given by arguments
  let dafnyBin = getDafnyPath args
  -- Get intended output directory as given by arguments
  let outputDir = getResultDir args
  -- Construct the color printing method
  let colorPrint = printColoredMsg args
  -- Construct Dafny file name from the path
  let parseFileName = unpack . replace (pack "/") (pack "__") . pack
  -- List all oracles in ascending order of precondition strength
  let oracles = [
          trivial,
          balancedFlowWP,
          balancedFlow
        ]
  -- Skip uninteresting examples
  _ <- if interesting p
    then return True
    else ioError $ userError "Example is not interesting."
  -- Abort early due to translation to back-end failure.
  encoding <- case irToBackend p of
    Ok encoding -> do
      return encoding
    Bad msg -> ioError $ userError $ "Back-end translation failed: " ++ msg
  -- Create output directory
  putStrLn "Successfully encoded VIRGo program to back-end."
  mkdir outputDir
  -- Write Dafny encoding for given oracle at destination file.
  let printDafny oracle = do
        -- Construct output file name from the input file path
        let outputFile = outputDir ++ "/" ++ parseFileName sourceFile ++ "-" ++ shortName oracle ++ ".dfy"
        -- Write to output file
        writeFile outputFile (show $ encodingToDafny oracle encoding)
        putStrLn $ "Dafny encoding found at: " ++ outputFile
        -- Get output file path
        return outputFile
  -- Run one oracle
  let verifyOne oracle = do
        -- Write Dafny to output file
        putStrLn $ "\nAttempting to run oracle " ++ oname oracle
        outputFile <- printDafny oracle
        -- Run Dafny on output file and fetch results
        (exitCode, outMsg, errMsg) <- readProcessWithExitCode dafnyBin [outputFile] ""
        case exitCode of
          ExitFailure 1 -> do
            -- Exit code 1 means command-line argument error
            colorPrint Red $ "Failed verification with Oracle " ++ oname oracle ++ "\n"
            putStrLn $ "Invalid command-line arguments: " ++ errMsg
            return False
          ExitFailure 2 -> do
            -- Exit code 2 means Dafny syntax error
            colorPrint Red $ "Failed verification with Oracle " ++ oname oracle ++ "\n"
            putStrLn $ "Syntax error: " ++ outMsg
            return False
          ExitFailure 3 -> do
            -- Exit code 3 means Dafny compilation error
            colorPrint Red $ "Failed verification with Oracle " ++ oname oracle ++ "\n"
            putStrLn $ "Compilation error: " ++ outMsg
            return False
          ExitFailure 4 -> do
            -- Exit code 4 means Dafny verification error
            colorPrint Red $ "Failed verification with Oracle " ++ oname oracle ++ "\n"
            putStrLn $ "Verification error: " ++ outMsg
            return False
          ExitFailure n -> do
            -- Unknown exit code
            colorPrint Red $ "Failed verification with Oracle " ++ oname oracle ++ "\n"
            putStrLn $ "Unknown exit code: " ++ show n
            putStrLn $ "OUT: " ++ outMsg
            putStrLn $ "ERR: " ++ errMsg
            return False
          ExitSuccess ->
            -- Successful verification must include this message
            if "Dafny program verifier finished with 1 verified, 0 errors" `isInfixOf` outMsg
              then do
                -- Verification successful
                colorPrint Green "Verification successful!\n"
                -- Check whether the precondition is unsatisfiable
                unsat <- unsatExpression dafnyBin encoding (realPrecondition oracle encoding)
                if unsat then do
                  -- If the precondition is unsatisfiable, partial deadlocks are guaranteed
                  colorPrint Red "Preconditions are unsatisfiable. Partial deadlock is guaranteed.\n"
                  putStrLn $ "Precondition: " ++ propositionalPrintExp (eSimplify $ realPrecondition oracle encoding)
                  return True
                else do
                  -- Otherwise, print the oracle success message, which includes the precondition for the non-trivial oracle.
                  putStrLn $ successMessage oracle encoding
                  return True
              else do
                -- Sometimes a 0 exit code does not mean success, but is caused by other issues.
                -- e.g., broken pipe due to missing C++ dependencies.
                putStrLn $ unlines [
                    "Verification did not produce expected message.",
                    "OUT: " ++ outMsg,
                    "ERR: " ++ errMsg
                  ]
                return False
  -- Iterate over every oracle in ascending order of strength
  let iterateOracles = \case
        [] -> ioError $ userError "Verification failed for all oracles."
        oracle : oracles' -> do
          verificationResult <- verifyOne oracle
          if verificationResult then return () else iterateOracles oracles'
  -- Skip verification if the `-skip-verification` flag was provided.
  if skipVerification args
    then do
      putStrLn "Emitting back-end without verification under the `WP Balanced Flow` strategy."
      _ <- printDafny balancedFlowWP
      return ()
    else iterateOracles oracles
