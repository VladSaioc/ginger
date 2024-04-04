module Pipeline.Verification.Runner (verify) where

import Control.Monad (unless)
import Data.List (isInfixOf, intercalate)
import Data.List.Split
import Data.Set qualified as S
import GHC.IO.Exception (ExitCode(ExitSuccess, ExitFailure))
import System.Console.ANSI
import System.Directory (createDirectory, doesDirectoryExist)
import System.Process (readProcessWithExitCode)
import System.Clock

import Backend.Profiler (sizeOfExpr)
import Backend.Simplifier (eSimplify)
import IR.Ast
import IR.Profiler (
  countVirgoFVOccurrences,
  countVirgoFVs,
  getVirgoParametricity,
  countVirgoFVOccurrencesLoop,
  countVirgoFVOccurrencesCaps,
  countVirgoFVOccurrencesIf,
  countVirgoFVOccurrencesAdd
  )
import Pipeline.IRTranslation.Close
import Pipeline.IRTranslation.Encoding
import Pipeline.IRTranslation.Workflow (irToBackend)
import Pipeline.Verification.Dafny
import Pipeline.Verification.Oracle
import Pipeline.Verification.TermVerifier (unsatExpression)
import Utilities.Args (getResultDir, getDafnyPath, skipVerification)
import Utilities.Color
import Utilities.Err
import Utilities.PrettyPrint

-- | Make a directory. Acts as no-op if directory already exists.
mkdir :: [Char] -> IO ()
mkdir dir = do
  putStrLn ("Checking directory " ++ dir ++ "...")
  dirExists <- doesDirectoryExist dir
  unless dirExists $ createDirectory dir

-- | Print results in a tabular format.
printTabular :: 𝑆 -> Oracle -> String -> IO ()
printTabular p oracle res = do
  putStrLn ""
  putStrLn "Final results (short)"
  putStrLn $ unwords ["[",
      intercalate " | " [res,
      shortName oracle,
      "FV occurrences: " ++ show (countVirgoFVOccurrences p),
      "Unique FVs: " ++ show (countVirgoFVs p),
      "FVs in loops: " ++ show (countVirgoFVOccurrencesLoop p),
      "FVs in caps: " ++ show (countVirgoFVOccurrencesCaps p),
      "FVs in ifs: " ++ show (countVirgoFVOccurrencesIf p),
      "FVs in adds: " ++ show (countVirgoFVOccurrencesAdd p),
      getVirgoParametricity p
      ],
     "]"]
  return ()

-- | Verify a VIRGo program encoding using Dafny.
verify :: [String] -> String -> 𝑆 -> IO ()
verify args sourceFile p = do
  -- Get Dafny binary as given by arguments
  let dafnyBin = getDafnyPath args
  -- Get intended output directory as given by arguments
  let outputDir = getResultDir args
  -- Construct the color printing method
  let colorPrint = printColoredMsg args
  -- Construct Dafny file name from the path
  let parseFileName = last . splitOn "/"
  -- List all oracles in ascending order of precondition strength
  let oracles = [
          trivial,
          balancedFlowWP,
          balancedFlow
        ]
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
        -- Prepare results
        let printResults = printTabular p oracle
        -- Write Dafny to output file
        putStrLn $ "\nAttempting to run oracle " ++ oname oracle
        outputFile <- printDafny oracle
        -- Get start timestamp
        start <- getTime Monotonic
        -- Run Dafny on output file and fetch results
        (exitCode, outMsg, errMsg) <- readProcessWithExitCode dafnyBin [outputFile] ""
        -- Get verification end timestamp
        end <- getTime Monotonic
        -- Compute verification time (ms)
        let t = toInteger (end - start) `div` 1000000
        putStrLn $ unwords [shortName oracle, "verification time:", show t ++ "ms"]
        case exitCode of
          ExitFailure 1 -> do
            -- Exit code 1 means command-line argument error
            colorPrint Red $ "Failed verification with Oracle " ++ oname oracle ++ "\n"
            putStrLn $ "Invalid command-line arguments: " ++ errMsg
            printResults  "CLI ARGUMENT ERROR"
            ioError $ userError "Unexpected command-line argument failure"
          ExitFailure 2 -> do
            -- Exit code 2 means Dafny syntax error
            colorPrint Red $ "Failed verification with Oracle " ++ oname oracle ++ "\n"
            putStrLn $ "Syntax error: " ++ outMsg
            printResults "SYNTAX ERROR"
            ioError $ userError "Unexpected syntax error"
          ExitFailure 3 -> do
            -- Exit code 3 means Dafny compilation error
            colorPrint Red $ "Failed verification with Oracle " ++ oname oracle ++ "\n"
            putStrLn $ "Compilation error: " ++ outMsg
            printResults "COMPILATION ERROR"
            ioError $ userError "Unexpected compilation error"
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
                  let (msg, fp) =
                        if noSendsFound encoding
                          then ("Preconditions are unsatisfiable. LIKELY FP! NO SENDS FOUND\n", "(FP) ")
                          else ("Preconditions are unsatisfiable. Partial deadlock is guaranteed.\n", "")
                  let (msg', fp') =
                        if noReceivesFound encoding
                          then ("Preconditions are unsatisfiable. LIKELY FP! NO RECEIVES FOUND\n", "(FP) ")
                          else (msg, fp)
                  let (msg'', fp'') =
                        if S.null (closes encoding)
                          then (msg', fp')
                          else ("Preconditions are unsatisfiable. LIKELY FP! CLOSE OPERATIONS FOUND\n", "(FP) ")
                  colorPrint Red msg''
                  putStrLn $ "Precondition: " ++ prettyPrint 0 (eSimplify $ realPrecondition oracle encoding)
                  printResults (fp'' ++ "UNSAT")
                  return True
                else do
                  -- Otherwise, print the oracle success message, which includes the precondition for the non-trivial oracle.
                  putStrLn $ successMessage oracle encoding
                  -- Print a message if the channel is closed.
                  putStrLn $ unlines (concatMap (closingChannelMsg $ comprojection encoding) $ S.toList (closes encoding))
                  printResults "SUCCESS"
                  return True
              else do
                -- Sometimes a 0 exit code does not mean success, but is caused by other issues.
                -- e.g., broken pipe due to missing C++ dependencies.
                putStrLn $ unlines [
                    "Verification did not produce expected message.",
                    "OUT: " ++ outMsg,
                    "ERR: " ++ errMsg
                  ]
                printResults "PIPE FAILURE"
                return False
  -- Iterate over every oracle in ascending order of strength
  let iterateOracles = \case
        [] -> do
          printTabular p Oracle { shortName = "-" } "VERIFICATION ERROR"
          ioError $ userError "Verification failed for all oracles."
        oracle : oracles' -> do
          verificationResult <- verifyOne oracle
          if verificationResult then return () else iterateOracles oracles'
  -- Skip verification if the `-skip-verification` flag was provided.
  if skipVerification args
    then do
      putStrLn "Emitting back-end without verification, under the `WP Balanced Flow` strategy."
      _ <- printDafny balancedFlowWP
      return ()
    else iterateOracles oracles
