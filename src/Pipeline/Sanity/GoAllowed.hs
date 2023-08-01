module Pipeline.Sanity.GoAllowed (allowed) where

import Data.Set qualified as S
import Go.Ast
import Utilities.Err
import Utilities.General
import Utilities.Position

type Ident = String

-- Context for checking whether a Go program
-- conforms to the restrictions imposed by Ginger
data Ctxt a = Ctxt
  { -- Set of channel names
    chans :: S.Set String,
    -- Set of loop variables
    loopVars :: S.Set String,
    -- Set of concurrency parameters
    commParams :: S.Set String,
    -- Set of mutable variables
    mutableVars :: S.Set String,
    -- Current loop depth
    loopDepth :: Int,
    -- Syntax of source language
    syntax :: a
  }
  deriving (Eq, Ord, Read, Show)

-- A program is allowed if all of its processes are allowed,
-- and they follow the pattern:
--  1. Check for allowed declarations
--  2. Check for allowed goroutine spawns
--  3. Check for allowed statements
allowed :: Prog -> Err ()
allowed (Prog ss) =
  let newCtxt =
        Ctxt
          { chans = S.empty,
            loopVars = S.empty,
            commParams = S.empty,
            mutableVars = S.empty,
            loopDepth = 0,
            syntax = ss
          }
   in allowedDeclarations newCtxt

-- Checks that all the declaration in a program are allowed.
allowedDeclarations :: Ctxt [Pos Stmt] -> Err ()
allowedDeclarations ρ = case syntax ρ of
  [] -> return ()
  Pos _ s : ss ->
    let ρ' = ρ {syntax = ss}
     in case s of
          -- Non-channel declarations are allowed and earmarked
          -- as concurrency parameters as long as the RHS is allowed.
          Decl _ e -> do
            cxs <- expVars e
            -- Proceed to next statement and earmark any concurrency parameters
            -- discovered in the RHS expression
            allowedDeclarations $ ρ' {commParams = S.union (commParams ρ') cxs}
          -- Channel declarations are allowed and earmarked
          -- as long as the capacity expression is allowed.
          Chan x e -> do
            cxs' <- expVars e
            let ρ2 =
                  ρ'
                    { -- Earmark channel name
                      chans = S.insert x (chans ρ'),
                      -- Earmark capacity-related concurrency parameters
                      commParams = S.union (commParams ρ') cxs'
                    }
            allowedDeclarations ρ2
          _ -> do
            _ <- allowedGoroutines ρ'
            return ()

-- Check that all goroutines in a Go program are allowed.
allowedGoroutines :: Ctxt [Pos Stmt] -> Err (Ctxt ())
allowedGoroutines ρ = case syntax ρ of
  [] -> return (ρ {syntax = ()})
  Pos _ s : ss ->
    let ρ' = ρ {syntax = ss}
     in case s of
          Go ss' -> do
            _ <- allowedGoroutines (ρ {syntax = ss'})
            allowedGoroutines ρ'
          _ -> allowedStmts ρ'

-- Check that all statements in a Go program consist strictly
-- of allowed features.
allowedStmts :: Ctxt [Pos Stmt] -> Err (Ctxt ())
allowedStmts ρ =
  let ok = return (ρ {syntax = ()})
   in case syntax ρ of
        [] -> ok
        Pos p s : ss ->
          let ρ' = ρ {syntax = ss}
              err = posErr p
              (!) prop msg = if prop then return () else err msg
           in case s of
                -- No declarations are allowed after the declaration phase
                -- is over.
                Decl x _ -> err $ "Unexpected declaration for: " ++ x
                Chan {} -> err "Unexpected channel declaration"
                -- Close operations are not supported (yet)
                Close {} -> err "Unexpected channel close"
                -- Irregular loops are not supported
                While {} -> err "Unexpected 'while' loop"
                -- Select statements are not supported (yet)
                Select {} -> allowedStmts (ρ {syntax = ss})
                -- Block statements are reduced to their contents
                Block ss' -> do
                  ctx'' <- allowedStmts (ρ {syntax = ss'})
                  allowedStmts (ctx'' {syntax = ss})
                -- Goroutine spawns are not allowed after the spawning
                -- phase is over.
                Go {} -> err "Unexpected 'go' statement."
                Atomic _ -> ok
                -- If statements are not supported (yet)
                If _ s1 s2 -> do
                  (loopDepth ρ == 0) ! "Found 'if' in loop"
                  ρ₁ <- allowedStmts (ρ {syntax = s1})
                  ρ₂ <- allowedStmts (ρ₁ {syntax = s2})
                  allowedStmts (ρ₂ {syntax = ss})
                -- Return statements are only supported outside loops.
                Return -> do
                  (loopDepth ρ == 0) ! "Found 'return' in loop"
                  ok
                -- Break statements are only supported outside loops.
                Break -> do
                  (loopDepth ρ == 0) ! "Found 'break' in loop"
                  ok
                For x e1 e2 _ body -> do
                  -- Do not allow nested loops.
                  (loopDepth ρ == 0) ! "Found nested loops"
                  -- Extract all concurrency-influecing variables (variables used
                  -- in loop bound expressions, and the loop index variable).
                  xs' <- binaryCons expVars S.union e1 e2
                  let xs = S.insert x xs'
                  let ρ'' =
                        ρ
                          { loopVars = S.union xs $ loopVars ρ,
                            loopDepth = 1,
                            syntax = body
                          }
                  -- Make sure that concurrency variables are not mutable
                  -- (except loop index variables in the context of their own loop).
                  S.disjoint xs (mutableVars ρ'') ! "Assignment to loop-relevant variable."
                  _ <- allowedStmts ρ''
                  allowedStmts (ρ'' {syntax = ss})
                -- Assignments are only allowed if they do not affect concurrency variables.
                As x e -> do
                  let x' = S.singleton x
                  S.disjoint x' (loopVars ρ) ! "Assignment to loop-relevant variable."
                  S.disjoint x' (commParams ρ) ! "Assignment to concurrency parameter."
                  _ <- expVars e
                  let ρ₂ = ρ' {mutableVars = S.union x' $ mutableVars ρ}
                  allowedStmts ρ₂
                -- Skip statements are always allowed.
                Skip -> allowedStmts ρ'

-- Collect all variables used in expressions.
expVars :: Exp -> Err (S.Set Ident)
expVars =
  let bin = binaryCons expVars S.union
   in \case
        CNum _ -> return S.empty
        CTrue -> return S.empty
        CFalse -> return S.empty
        And e1 e2 -> bin e1 e2
        Or e1 e2 -> bin e1 e2
        Eq e1 e2 -> bin e1 e2
        Ne e1 e2 -> bin e1 e2
        Le e1 e2 -> bin e1 e2
        Lt e1 e2 -> bin e1 e2
        Ge e1 e2 -> bin e1 e2
        Gt e1 e2 -> bin e1 e2
        Plus e1 e2 -> bin e1 e2
        Minus e1 e2 -> bin e1 e2
        Mult e1 e2 -> bin e1 e2
        Div e1 e2 -> bin e1 e2
        Neg e -> expVars e
        Not e -> expVars e
        Var x -> return $ S.singleton x
