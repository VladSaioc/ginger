module Pipeline.Sanity.PromelaAllowed (allowed) where

import Data.List qualified as L
import Data.Set qualified as S

import Promela.Ast
import Utilities.Err
import Utilities.General
import Utilities.Position

-- | Context for checking program permissivity
data Ctxt a = Ctxt
  { -- A set of channel names
    chans :: S.Set String,
    -- Set of loop variables
    loopVars :: S.Set String,
    -- Set of communication parameters
    commParams :: S.Set String,
    -- Set of mutable variables
    mutableVars :: S.Set String,
    -- Loop depth
    loopDepth :: Int,
    -- Syntax to check
    syntax :: a
  }
  deriving (Eq, Ord, Read, Show)

-- | Checks whether the Promela program only uses covered features.
-- All of its modules must be allowed for the whole program to be allowed.
allowed :: Spec -> Err [()]
allowed (Spec ms) = results (map allowedModule ms)

-- | Checks whether a top-level declaration uses only allowed features.
-- Procedures and the initial process are only allowed if:
--  1. All their declarations appear first
--  2. All their goroutines appear second
--  3. The remaining statements use only allowed features.
allowedModule :: Module -> Err ()
allowedModule =
  let newCtxt s =
        Ctxt
          { -- Initial variable sets are empty
            chans = S.empty,
            loopVars = S.empty,
            commParams = S.empty,
            mutableVars = S.empty,
            -- Initial loop depth is 0
            loopDepth = 0,
            syntax = s
          }
   in \case
        -- The initial process is allowed only if its statements are
        -- used in permissible patterns.
        Init ss -> allowedDeclarations (newCtxt ss)
        -- A named procedure is allowed only if its statements are
        -- used in permissible patterns.
        Proc _ ps ss ->
          let ctxt = newCtxt ss
              ctxt' = ctxt {chans = S.union (chans ctxt) (paramChans ps)}
           in allowedDeclarations ctxt'
        _ -> return ()

-- Construct set of channel names from procedure parameters.
paramChans :: [Param] -> S.Set Ident
paramChans =
  foldl
    ( \s -> \case
        (x, TChan) -> S.insert x s
        _ -> s
    )
    S.empty

-- Checks that the procedure uses only allowed declaration patterns.
-- Collects all declared channel names.
allowedDeclarations :: Ctxt [Pos Stmt] -> Err ()
allowedDeclarations ctxt = case syntax ctxt of
  -- If no more statements are found, stop
  [] -> return ()
  Pos _ s : ss -> case s of
    -- Check that declaration is allowed, and save channel name.
    Decl x t me -> do
      let ctxt' = case t of
            TChan -> ctxt {chans = S.insert x (chans ctxt)}
            _ -> ctxt
      cxs' <- maybe (return S.empty) expVars me
      let ctxt'' =
            ctxt'
              { syntax = ss,
                commParams = cxs'
              }
      allowedDeclarations ctxt''
    -- If the next statement is not a declaration, switch
    -- to checking for permissible goroutine declarations.
    _ -> allowedGoroutines ctxt

-- Check that the procedure uses only allowed goroutine declarations.
allowedGoroutines :: Ctxt [Pos Stmt] -> Err ()
allowedGoroutines ctxt = case syntax ctxt of
  -- If no more statements are found, stop
  [] -> return ()
  Pos _ s : ss -> case s of
    -- Goroutine spawning is encoded as a 'run' expression statement.
    ExpS (Run _ es) -> do
      _ <- results (L.map expVars es)
      allowedGoroutines (ctxt {syntax = ss})
    -- If no more goroutines are spawned, check for allowed statements.
    _ -> allowedStmts ctxt {syntax = ss}

-- Check that all the statements in a procedure are allowed.
allowedStmts :: Ctxt [Pos Stmt] -> Err ()
allowedStmts ctxt =
  let ok = return ()
   in case syntax ctxt of
        [] -> ok
        Pos p s : ss ->
          let err = posErr p
              (!) prop msg = if prop then return () else err msg
              -- Channel operations are allowed as long as they do not
              -- impact concurrency parameters.
              chanOp es msg = do
                es' <- foldMonad expVars S.empty S.union es
                S.disjoint (loopVars ctxt) es' ! ("Loop-relevant value " ++ msg ++ " channel")
                ok
           in case s of
                -- Declarations are not allowed at this point
                Decl {} -> err "Unexpected declaration"
                -- If statements are not allowed (yet)
                If {} -> err "Unexpected IF statement"
                -- Do statements are not allowed
                Do {} -> err "Unexpected DO statement"
                -- For statements are only allowed if:
                -- 1. The loop variable is not mutable beyond the loop itself
                -- 2. There are no nested loops
                -- 3. Its body only consists of allowed statements
                For r body -> do
                  xs <- rangeVars r
                  (loopDepth ctxt == 0) ! "Found nested loops"
                  let ctxt' =
                        ctxt
                          { loopVars = S.union xs (loopVars ctxt),
                            -- Inform the context we are in a nested loop
                            loopDepth = 1,
                            syntax = body
                          }
                  S.disjoint xs (mutableVars ctxt') ! "Assignment to loop-relevant variable."
                  _ <- allowedStmts ctxt'
                  allowedStmts (ctxt {syntax = ss})
                -- Assignments are allowed only as long as they do
                -- not affect concurrency parameters or loop variables.
                As x e -> do
                  x' <- lvalVars x
                  S.disjoint x' (loopVars ctxt) ! "Assignment to loop-relevant variable."
                  S.disjoint x' (commParams ctxt) ! "Assignment to concurrency parameter."
                  _ <- expVars e
                  -- Earmark assignee variable as mutable.
                  let ctxt' = ctxt {mutableVars = S.union x' $ mutableVars ctxt}
                  allowedStmts (ctxt' {syntax = ss})
                -- 'goto' statements are not allowed.
                Goto lbl -> err ("Unexpected \"goto: " ++ lbl ++ "\"")
                -- 'break' statements are not allowed.
                Break -> err "Unexpected break"
                -- 'skip' statements are allowed
                Skip -> allowedStmts (ctxt {syntax = ss})
                -- Labeled statements are allowed.
                -- Without a corresponding 'goto', they are simple no-ops.
                Label _ -> allowedStmts (ctxt {syntax = ss})
                -- Send operations are allowed only as long as their payloads
                -- do not include concurrency parameters.
                Send _ es -> chanOp es "sent to"
                -- Receive operations are allowed only as long as they
                -- do not write to concurrency parameters.
                Recv _ es -> chanOp es "received from"
                Assert _ -> allowedStmts ctxt
                -- Expressions are only allowed if no 'run' statements are present.
                ExpS e -> do
                  _ <- expVars e
                  allowedStmts ctxt

-- Check that a range expression is allowed, and returns
-- the set of relevant variables, including loop index and
-- variables used for bounds. These variables must not be
-- mutable by any other means, except the loop.
rangeVars :: Range -> Err (S.Set Ident)
rangeVars = \case
  -- Numerically bounded iteration is allowed.
  Between x e1 e2 -> do
    s <- binaryCons expVars S.union e1 e2
    return (S.insert x s)
  -- Collection iteration is not allowed.
  In {} -> Bad "Unexpected range over collection"

-- Construct set of free variables from an expression,
-- and check that all sub-expressions are also allowed.
expVars :: Exp -> Err (S.Set Ident)
expVars =
  let bin = binaryCons expVars S.union
   in \case
        Chan e -> expVars e
        Const _ -> return S.empty
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
        EVar x -> lvalVars x
        -- Goroutine forking cannot happen inside complex expression
        Run f _ -> Bad $ "Unexpected RUN expression of " ++ f
        Len _ -> Bad "Unexpected \"len\" expression."

-- Only plain variables are allowed as L-values.
lvalVars :: LVal -> Err (S.Set Ident)
lvalVars = \case
  Var x -> return (S.singleton x)
  _ -> Bad "Unexpected non-variable ℓ-value."
