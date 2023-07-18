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
allowedDeclarations ctx = case syntax ctx of
  [] -> return ()
  Pos _ s : ss ->
    let ctx' = ctx {syntax = ss}
     in case s of
          -- Non-channel declarations are allowed and earmarked
          -- as concurrency parameters as long as the RHS is allowed.
          Decl _ e -> do
            cxs' <- expVars e
            -- Proceed to next statement and earmark any concurrency parameters
            -- discovered in the RHS expression
            allowedDeclarations ctx' {commParams = S.union (commParams ctx') cxs'}
          -- Channel declarations are allowed and earmarked
          -- as long as the capacity expression is allowed.
          Chan x e -> do
            cxs' <- expVars e
            let ctx2 =
                  ctx'
                    { -- Earmark channel name
                      chans = S.insert x (chans ctx'),
                      -- Earmark capacity-related concurrency parameters
                      commParams = S.union (commParams ctx') cxs'
                    }
            allowedDeclarations ctx2
          _ -> do
            _ <- allowedGoroutines ctx'
            return ()

-- Check that all goroutines in a Go program are allowed.
allowedGoroutines :: Ctxt [Pos Stmt] -> Err (Ctxt ())
allowedGoroutines ctx = case syntax ctx of
  [] -> return (ctx {syntax = ()})
  Pos _ s : ss ->
    let ctx' = ctx {syntax = ss}
     in case s of
          Go ss' -> do
            _ <- allowedGoroutines (ctx {syntax = ss'})
            allowedGoroutines ctx'
          _ -> allowedStmts ctx'

-- Check that all statements in a Go program consist strictly
-- of allowed features.
allowedStmts :: Ctxt [Pos Stmt] -> Err (Ctxt ())
allowedStmts ctx =
  let ok = return (ctx {syntax = ()})
   in case syntax ctx of
        [] -> ok
        Pos p s : ss ->
          let ctx' = ctx {syntax = ss}
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
                Select {} -> err "Unexpected 'select' statement"
                -- Block statements are reduced to their contents
                Block ss' -> do
                  ctx'' <- allowedStmts (ctx {syntax = ss'})
                  allowedStmts (ctx'' {syntax = ss})
                -- Goroutine spawns are not allowed after the spawning
                -- phase is over.
                Go {} -> err "Unexpected 'go' statement."
                Atomic _ -> ok
                -- If statements are not supported (yet)
                If {} -> err "Unexpected IF statement"
                -- Return statements are only supported outside loops.
                Return -> do
                  (loopDepth ctx == 0) ! "Found 'return' in loop"
                  ok
                -- Break statements are only supported outside loops.
                Break -> do
                  (loopDepth ctx == 0) ! "Found 'break' in loop"
                  ok
                For x e1 e2 _ body -> do
                  -- Do not allow nested loops.
                  (loopDepth ctx == 0) ! "Found nested loops"
                  -- Extract all concurrency-influecing variables (variables used
                  -- in loop bound expressions, and the loop index variable).
                  xs' <- binaryCons expVars S.union e1 e2
                  let xs = S.insert x xs'
                  let ctxt' =
                        ctx
                          { loopVars = S.union xs (loopVars ctx),
                            loopDepth = 1,
                            syntax = body
                          }
                  -- Make sure that concurrency variables are not mutable
                  -- (except loop index variables in the context of their own loop).
                  S.disjoint xs (mutableVars ctxt') ! "Assignment to loop-relevant variable."
                  _ <- allowedStmts ctxt'
                  allowedStmts (ctx {syntax = ss})
                -- Assignments are only allowed if they do not affect concurrency variables.
                As x e -> do
                  let x' = S.singleton x
                  S.disjoint x' (loopVars ctx) ! "Assignment to loop-relevant variable."
                  S.disjoint x' (commParams ctx) ! "Assignment to concurrency parameter."
                  _ <- expVars e
                  let ctx2 = ctx' {mutableVars = S.union x' $ mutableVars ctx}
                  allowedStmts ctx2
                -- Skip statements are always allowed.
                Skip -> allowedStmts ctx'

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
