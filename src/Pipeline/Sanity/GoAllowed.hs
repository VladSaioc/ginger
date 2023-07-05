module Pipeline.Sanity.GoAllowed (allowed) where

import Data.Set qualified as S
import Go.Ast
import Utilities.Err
import Utilities.General
import Utilities.Position

type Ident = String

data Ctxt a = Ctxt
  { chans :: S.Set String,
    loopVars :: S.Set String,
    commParams :: S.Set String,
    mutableVars :: S.Set String,
    loopDepth :: Int,
    syntax :: a
  }
  deriving (Eq, Ord, Read, Show)

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

allowedDeclarations :: Ctxt [Pos Stmt] -> Err ()
allowedDeclarations ctx = case syntax ctx of
  [] -> return ()
  Pos _ s : ss ->
    let ctx' = ctx {syntax = ss}
     in case s of
          Chan x e -> do
            cxs' <- expVars e
            let ctx2 =
                  ctx'
                    { chans = S.insert x (chans ctx),
                      commParams = cxs'
                    }
            allowedDeclarations ctx2
          _ -> allowedGoroutines ctx'

allowedGoroutines :: Ctxt [Pos Stmt] -> Err ()
allowedGoroutines ctx = case syntax ctx of
  [] -> return ()
  Pos _ s : ss ->
    let ctx' = ctx {syntax = ss}
     in case s of
          Go ss' -> do
            _ <- allowedGoroutines (ctx {syntax = ss'})
            allowedGoroutines ctx'
          _ -> allowedStmts ctx'

allowedStmts :: Ctxt [Pos Stmt] -> Err ()
allowedStmts ctx =
  let ok = return ()
   in case syntax ctx of
        [] -> ok
        Pos p s : ss ->
          let ctx' = ctx {syntax = ss}
              err = posErr p
              (!) prop msg = if prop then return () else err msg
           in case s of
                Decl {} -> err "Unexpected declaration"
                Chan {} -> err "Unexpected channel declaration"
                Close {} -> err "Unexpected channel close"
                While {} -> err "Unexpected 'while' loop"
                Select {} -> err "Unexpected 'select' statement"
                Block ss' -> allowedStmts (ctx {syntax = ss' ++ ss})
                Go {} -> err "Unexpected 'go' statement."
                Atomic _ -> ok
                If {} -> err "Unexpected IF statement"
                Return -> (loopDepth ctx == 0) ! "Found 'return' in loop"
                Break -> (loopDepth ctx == 0) ! "Found 'break' in loop"
                For x e1 e2 _ body -> do
                  (loopDepth ctx == 0) ! "Found nested loops"
                  xs' <- binaryCons expVars S.union e1 e2
                  let xs = S.insert x xs'
                  let ctxt' =
                        ctx
                          { loopVars = S.union xs (loopVars ctx),
                            loopDepth = 1,
                            syntax = body
                          }
                  S.disjoint xs (mutableVars ctxt') ! "Assignment to loop-relevant variable."
                  _ <- allowedStmts ctxt'
                  allowedStmts (ctx {syntax = ss})
                As x e -> do
                  let x' = S.singleton x
                  S.disjoint x' (loopVars ctx) ! "Assignment to loop-relevant variable."
                  S.disjoint x' (commParams ctx) ! "Assignment to concurrency parameter."
                  _ <- expVars e
                  let ctx2 = ctx' {mutableVars = S.union x' $ mutableVars ctx}
                  allowedStmts ctx2
                Skip -> allowedStmts ctx'

expVars :: Exp -> Err (S.Set Ident)
expVars =
  let bin = binaryCons expVars S.union
   in \case
        CNum _ -> return S.empty
        Go.Ast.True -> return S.empty
        Go.Ast.False -> return S.empty
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
