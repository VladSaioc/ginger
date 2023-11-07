module Pipeline.Verification.TermVerifier where

import Data.List (isInfixOf)
import Data.Map qualified as M
import GHC.IO.Exception (ExitCode(ExitSuccess, ExitFailure))
import System.Process (readProcessWithExitCode)

import Backend.Ast
import Backend.Utilities
import Pipeline.IRTranslation.Encoding
import Pipeline.Verification.Dafny (iterationsFunc)


-- | Run Dafny on a single program.
runDafny :: Show a => FilePath -> a -> IO Bool
runDafny dafnyBin program = do
  (exitCode, successMsg, _) <- readProcessWithExitCode dafnyBin ["/stdin"] $ show program
  case exitCode of
    ExitFailure _ -> return False
    ExitSuccess -> return $ "Dafny program verifier finished with 1 verified, 0 errors" `isInfixOf` successMsg

-- | Verify that a Dafny expression ensures a certain property relative to the encoding (free value and type variables).
-- The property should produce a boolean typed expression. Otherwise, the produced program will produce a type error.
verifyExpression :: String -> Encoding -> Exp -> (Exp -> Exp) -> IO Bool
verifyExpression dafnyBin Encoding { typeenv = 𝛾, typevars = ts } e prop =
  let term = Program [
        FDecl Function {
          funcHoare = HoareWrap {
            ghost = True,
            name = "term",
            types = ts,
            params = M.toList 𝛾,
            decreases = [],
            requires = [],
            ensures = [prop e]
          },
          yields = Tuple [],
          funcBody = ETuple []
        },
        FDecl iterationsFunc
        ]
  in runDafny dafnyBin term

-- | Verifies whether a term is unsatisfiable by checking that it is equivalent to 'false'.
unsatExpression :: String -> Encoding -> Exp -> IO Bool
unsatExpression dafnyBin encoding e = verifyExpression dafnyBin encoding e ((False ?) :<==>)

-- | Verifies whether a term is tautological by checking that it is equivalent to 'true'.
tautExpression :: String -> Encoding -> Exp -> IO Bool
tautExpression dafnyBin encoding e = verifyExpression dafnyBin encoding e ((True ?) :<==>)
