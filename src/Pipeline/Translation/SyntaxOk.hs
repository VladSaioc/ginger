module Pipeline.Translation.SyntaxOk (allowed) where

import Data.Set qualified as S
import Promela.Ast
import Utilities.Err
import Utilities.Position

data Ctxt a = Ctxt
  { chans :: S.Set String,
    loopVars :: S.Set String,
    mutableVars :: S.Set String,
    loopDepth :: Int,
    syntax :: a
  }
  deriving (Eq, Ord, Read, Show)

allowed :: Spec -> Bool
allowed (Spec ms) = all ((== Ok ()) . allowedModule) ms

allowedModule :: Module -> Err ()
allowedModule =
  let newCtxt s =
        Ctxt
          { chans = S.empty,
            loopVars = S.empty,
            mutableVars = S.empty,
            loopDepth = 0,
            syntax = s
          }
   in \case
        Init ss -> allowedDeclarations (newCtxt ss)
        Proc _ ps ss ->
          let ctxt = newCtxt ss
              ctxt' = ctxt {chans = S.union (chans ctxt) (paramChans ps)}
           in allowedDeclarations ctxt'
        _ -> Ok ()

paramChans :: [Param] -> S.Set Ident
paramChans =
  foldl
    ( \s -> \case
        (x, TChan) -> S.insert x s
        _ -> s
    )
    S.empty

allowedDeclarations :: Ctxt [Pos Stmt] -> Err ()
allowedDeclarations ctxt = case syntax ctxt of
  [] -> Ok ()
  Pos _ s : ss -> case s of
    Decl x t me -> do
      let ctxt' = case t of
            TChan -> ctxt {chans = S.insert x (chans ctxt)}
            _ -> ctxt
      _ <- maybe (Ok S.empty) expVars me
      allowedDeclarations ctxt' {syntax = ss}
    _ -> allowedGoroutines ctxt

allowedGoroutines :: Ctxt [Pos Stmt] -> Err ()
allowedGoroutines ctxt = case syntax ctxt of
  [] -> Ok ()
  Pos _ s : ss -> case s of
    ExpS (Run _ es) -> do
      let expsOk =
            Prelude.foldl
              ( \ms e -> do
                  _ <- ms
                  _ <- expVars e
                  return ()
              )
              (Ok ())
              es
      _ <- expsOk
      allowedGoroutines (ctxt {syntax = ss})
    _ -> allowedStmts ctxt {syntax = ss}

allowedStmts :: Ctxt [Pos Stmt] -> Err ()
allowedStmts ctxt =
  let ok = Ok ()
   in case syntax ctxt of
        [] -> ok
        Pos p s : ss ->
          let err msg = Bad (":" ++ show p ++ ": " ++ msg)
              (!) prop msg = if prop then Ok () else err msg
              roll process start combine =
                Prelude.foldl
                  ( \ms e -> do
                      s1 <- ms
                      s2 <- process e
                      return (combine [s1, s2])
                  )
                  (Ok start)
           in case s of
                Decl {} -> err "Unexpected declaration"
                If {} -> err "Unexpected IF statement"
                Do {} -> err "Unexpected DO statement"
                For r body -> do
                  xs <- rangeVars r
                  (loopDepth ctxt == 0) ! "Found nested loops"
                  let ctxt' =
                        ctxt
                          { loopVars = S.union xs (loopVars ctxt),
                            loopDepth = 1,
                            syntax = body
                          }
                  _ <- allowedStmts ctxt'
                  allowedStmts (ctxt {syntax = ss})
                As x e -> do
                  x' <- lvalVars x
                  S.disjoint x' (loopVars ctxt) ! "Assignment to loop-relevant variable."
                  _ <- expVars e
                  allowedStmts (ctxt {syntax = ss})
                Goto lbl -> err ("Unexpected \"goto: " ++ lbl ++ "\"")
                Break -> err "Unexpected break"
                Skip -> allowedStmts (ctxt {syntax = ss})
                Label lbl _ -> err ("Unexpected label: " ++ lbl)
                Send _ es -> do
                  es' <- roll expVars S.empty S.unions es
                  S.disjoint (loopVars ctxt) es' ! "Loop-relevant value sent to channel"
                  ok
                Rcv _ es -> do
                  es' <- roll expVars S.empty S.unions es
                  S.disjoint (loopVars ctxt) es' ! "Loop-relevant value received from channel"
                  ok
                Assert _ -> allowedStmts ctxt
                ExpS e -> do
                  _ <- expVars e
                  allowedStmts ctxt

rangeVars :: Range -> Err (S.Set Ident)
rangeVars = \case
  Between x e1 e2 -> do
    s1 <- expVars e1
    s2 <- expVars e2
    return (S.unions [S.singleton x, s1, s2])
  In {} -> Bad "Unexpected range over collection"

expVars :: Exp -> Err (S.Set Ident)
expVars =
  let bin e1 e2 = do
        s1 <- expVars e1
        s2 <- expVars e2
        return (S.union s1 s2)
   in \case
        Chan _ -> Ok S.empty
        Const _ -> Ok S.empty
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
        Run f _ -> Bad ("Unexpected RUN expression of " ++ f)
        Len _ -> Bad "Unexpected \"len\" expression."

lvalVars :: LVal -> Err (S.Set Ident)
lvalVars = \case
  Var x -> Ok (S.singleton x)
  _ -> Bad "Unexpected non-variable ℓ-value."