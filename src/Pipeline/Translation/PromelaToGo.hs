module Pipeline.Translation.PromelaToGo (getGo) where

import Control.Monad
import Data.Map qualified as M
import Go.Ast qualified as P'
import Go.Utilities (flipIfs)
import Pipeline.Callgraph (getCG)
import Promela.Ast qualified as P
import Promela.Utilities
import Utilities.Err
import Utilities.General
import Utilities.Position

data Ctxt a b = Ctxt
  { syntax :: a,
    cg :: M.Map String P.Module,
    cchain :: String,
    varenv :: M.Map String String,
    calls :: M.Map String Int,
    chenv :: M.Map String String,
    chans :: M.Map String P'.Exp,
    curr :: b
  }
  deriving (Eq, Ord, Read, Show)

wrapCtx :: Ctxt a b -> Err (Ctxt () b)
wrapCtx ctx = return ctx {syntax = ()}

(<:) :: Ctxt a c -> b -> Ctxt a b
(<:) ctx b = ctx {curr = b}

(>:) :: a -> Ctxt c b -> Ctxt a b
(>:) a ctx = ctx {syntax = a}

-- Reconstruct a Go AST from the Promela encoding.
-- Explode program by following call edges.
--
-- IMPORTANT: Assumes alpha conversion and absence of recursion.
getGo :: P.Spec -> Err P'.Prog
getGo p@(P.Spec ms) =
  let getFV venv = \case
        P.TopDecl x _ _ -> M.insert x x venv
        _ -> venv
      getInitVals decls = \case
        P.TopDecl x _ v -> case translateVal v of
          Just v' -> Pos 0 (P'.Decl x v') : decls
          Nothing -> decls
        _ -> decls
   in do
        let initStmts = Prelude.foldl getInitVals [] ms
        let ctx =
              Ctxt
                { syntax = getInit p,
                  cg = getCG p,
                  cchain = "",
                  varenv = Prelude.foldl getFV M.empty ms,
                  chans = M.empty,
                  chenv = M.empty,
                  calls = M.empty,
                  curr = initStmts
                }
        ctx' <- translateStatements ctx
        return $ P'.Prog (curr ctx')

translateStatements :: Ctxt [Pos P.Stmt] [Pos P'.Stmt] -> Err (Ctxt () [Pos P'.Stmt])
translateStatements ctx = case syntax ctx of
  [] -> wrapCtx $ ctx <: reverse (curr ctx)
  Pos p s : ss ->
    let translateExp = translateExpPos p
        err = posErr p
        addOp op = do
          ctx' <- translateOp (Pos p op >: ctx)
          let stm = curr ctx' : curr ctx
          translateStatements (ss >: ctx' <: stm)
     in case s of
          P.As (P.Var x) e -> do
            x' <-
              ( case M.lookup x (varenv ctx) of
                  Just x' -> return x'
                  Nothing -> err $ "[INVALID VARIABLE] binding not found for: " ++ x
                )
            e' <- translateExp (varenv ctx) e
            translateStatements (ss >: ctx <: (Pos p (P'.As x' e') : curr ctx))
          P.As _ _ -> err "[INVALID ASSIGNMENT] unrecognized write to complex data structure"
          P.Send {} -> addOp s
          P.Recv {} -> addOp s
          -- Assert statements are irrelevant
          P.Assert _ -> translateStatements (ss >: ctx)
          -- Skip statements are irrelevant
          P.Skip -> translateStatements (ss >: ctx)
          -- 'label:' statements are not a feature.
          P.Label _ -> translateStatements (ss >: ctx)
          -- Can discard the continuation of 'break', since it is unreachable.
          P.Break -> translateStatements ([] >: ctx <: (Pos p P'.Break : curr ctx))
          -- Can discard the continuation of 'goto stop_process', since it is unreachable.
          P.Goto "stop_process" -> translateStatements ([] >: ctx <: (Pos p P'.Return : curr ctx))
          P.Goto l -> err $ "Unexpected statement: goto " ++ l
          P.If os mels ->
            let -- First try to check whether the if statement is a select statement
                notSelectMessage = "if is not select"

                makeSelect ctx' = case curr ctx' of
                  P'.Select cs def ->
                    let addCommCase op c ss' = do
                          ctx'' <- translateStatements (ss' >: ctx' <: [])
                          let select = P'.Select ((Pos p $ op c, curr ctx'') : cs) def
                          wrapCtx $ ctx'' <: select
                     in \case
                          -- The 'default -> ...' branch discovered is considered
                          -- a 'default' case.
                          (Pos _ (P.ExpS (P.EVar (P.Var "default"))), ss') -> do
                            -- If more than one 'default' case exists,
                            -- then it's not a select statement.
                            ctx'' <- case def of
                              Nothing -> translateStatements (ss' >: ctx' <: [])
                              Just _ -> Bad notSelectMessage
                            let def' = curr ctx''
                            let select = P'.Select cs $ Just def'
                            wrapCtx $ ctx'' <: select
                          -- Cases of the form: 'c!_ -> ...'.
                          (Pos _ (P.Send (P.Var c) _), ss') -> addCommCase P'.Send c ss'
                          -- Cases of the form: 'c?_ -> ...'.
                          (Pos _ (P.Recv (P.Var c) _), ss') -> addCommCase P'.Recv c ss'
                          -- Cases of the form: 'true -> ...' are considered
                          -- operations on always potentially enabled channels.
                          -- This applies to timeouts or context channels.
                          (Pos p' (P.ExpS (P.Const (P.VBool True))), ss') -> do
                            ctx'' <- translateStatements (ss' >: ctx' <: [])
                            let caseClause = Pos p' P'.Star
                            let select = P'.Select ((caseClause, curr ctx'') : cs) def
                            wrapCtx $ ctx'' <: select
                          -- Cases of the form 'c[_]!_ -> ...' or 'x.c!_ -> ...' are not
                          -- covered features.
                          (Pos _ (P.Send _ _), _) -> Bad "[INVALID SEND]: Operations on channel in aggregate data structures are not supported"
                          -- Cases of the form 'c[_]?_ -> ...' or 'x.c?_ -> ...' are not
                          -- covered features.
                          (Pos _ (P.Recv _ _), _) -> Bad "[INVALID RECEIVE]: Operations on channel in aggregate data structures are not supported"
                          _ -> Bad notSelectMessage
                  _ -> const $ Bad "Impossible case. Select transform lost 'select' statement"

                makeIf ctx1@Ctxt {curr = ifSoFar, varenv} = \case
                  (Pos p' (P.ExpS e), ss') -> do
                    e' <- translateExpPos p' varenv e
                    ctx2 <- translateStatements (ss' >: ctx1 <: [])
                    let ss'' = curr ctx2
                    let body = P'.If e' ss'' [Pos p' ifSoFar]
                    wrapCtx $ ctx2 <: body

                  -- Cases of the form 'c[_]!_ -> ...' or 'x.c!_ -> ...' are not
                  -- covered features.
                  (Pos _ (P.Send _ _), _) -> Bad "[INVALID SEND]: Operations on channel in perceived 'if'-statement"
                  -- Cases of the form 'c[_]?_ -> ...' or 'x.c?_ -> ...' are not
                  -- covered features.
                  (Pos _ (P.Recv _ _), _) -> Bad "[INVALID RECEIVE]: Operations on channel in perceived 'if'-statement"
                  _ -> Bad "[INVALID IF BRANCH]: If statement has unrecognizable branch."

                freshSelect = P'.Select [] Nothing
             in case foldM makeSelect (() >: ctx <: freshSelect) os of
                  Bad msg -> do
                    _ <- if msg == notSelectMessage then return () else err msg
                    ctx1 <- case mels of
                      Nothing -> wrapCtx $ ctx <: []
                      Just els' -> translateStatements (els' >: ctx <: [])
                    let els = curr ctx1
                    let body = P'.Block els
                    ctx2 <- foldM makeIf (() >: ctx1 <: body) os
                    let ifStmt = flipIfs $ Pos p (curr ctx2)
                    let ctx3 = ctx2 <: (ifStmt : curr ctx)
                    translateStatements $ ss >: ctx3
                  Ok ctx' -> case curr ctx' of
                    P'.Select cs def -> do
                      let select' = P'.Select (reverse cs) def
                      translateStatements $ ss >: ctx' <: (Pos p select' : curr ctx)
                    _ -> err "Impossible case. Select transform did not produce select statement."
          P.Decl x t me ->
            let primitiveDecl zero = do
                  let x' = cchain ctx ++ x
                  rhs <- case me of
                    Just e -> translateExp (varenv ctx) e
                    Nothing -> return zero
                  let ctx'' =
                        ctx
                          { varenv = M.insert x x' $ varenv ctx,
                            curr = Pos p (P'.Decl x rhs) : curr ctx
                          }
                  translateStatements (ss >: ctx'')
             in case t of
                  P.TChan ->
                    case me of
                      Just (P.Chan e) -> do
                        e' <- translateExp (varenv ctx) e
                        let chdecl = Pos p $ P'.Chan x e'
                        let ctx' =
                              ctx
                                { chans = M.insert x e' (chans ctx),
                                  chenv = M.insert x x (chenv ctx),
                                  curr = chdecl : curr ctx
                                }
                        translateStatements $ ss >: ctx'
                      _ -> err $ "Channel " ++ x ++ " has no capacity."
                  P.TInt -> primitiveDecl $ P'.CNum 0
                  P.TBool -> primitiveDecl P'.False
                  P.TNamed _ -> translateStatements $ ss >: ctx
          P.ExpS (P.Run f es) ->
            case M.lookup f (cg ctx) of
              Just (P.Proc _ ps ss') -> do
                let calls' = case M.lookup f $ calls ctx of
                      Just n -> M.insert f (n + 1) $ calls ctx
                      Nothing -> M.insert f 0 $ calls ctx
                let f' = case M.lookup f $ calls ctx of
                      Just n -> f ++ show n
                      Nothing -> f ++ "0"
                let chainPrefix = cchain ctx ++ f' ++ "_"
                let pes = zip ps es
                    addVarInit ((x, t), e) =
                      let x' = chainPrefix ++ x
                          addExp = translateExp (varenv ctx) >=> return . (: []) . Pos p . P'.Decl x'
                       in case t of
                            P.TChan -> return []
                            P.TNamed _ -> err "Unexpected named type"
                            P.TBool -> addExp e
                            P.TInt -> addExp e
                    addVarName ve (x, t) =
                      case t of
                        P.TInt -> M.insert x (chainPrefix ++ x) ve
                        P.TBool -> M.insert x (chainPrefix ++ x) ve
                        _ -> ve
                let addCh ce ((a, t), e) =
                      case (t, e) of
                        (P.TChan, P.EVar (P.Var c)) -> M.insert a c ce
                        _ -> ce
                inits <- foldMonad addVarInit [] (++) pes
                let ctx1 =
                      Ctxt
                        { syntax = ss',
                          cg = cg ctx,
                          cchain = cchain ctx ++ f ++ "_",
                          calls = M.empty,
                          varenv = Prelude.foldl addVarName (varenv ctx) ps,
                          chenv = Prelude.foldl addCh M.empty pes,
                          chans = chans ctx,
                          curr = inits
                        }
                ctx2 <- translateStatements ctx1
                let ctx3 =
                      ctx
                        { chans = chans ctx2,
                          calls = calls',
                          curr = Pos p (P'.Go (curr ctx2)) : curr ctx
                        }
                translateStatements (ss >: ctx3)
              _ -> err $ "[INVALID FUNCTION] " ++ f ++ " not in call-graph."
          -- Non-run expression are considered irrelevant (Only checks for the absence of calls)
          P.ExpS e -> do
            _ <- translateExp (varenv ctx) e
            translateStatements (ss >: ctx)
          P.For r ss' -> do
            (x, e1', e2') <- translateRange p (varenv ctx) r
            ctx' <- translateStatements (ss' >: ctx <: [])
            let ctx'' = ctx <: (Pos p (P'.For x e1' e2' P'.Inc (curr ctx')) : curr ctx)
            translateStatements (ss >: ctx'')
          -- P.Do [s] Nothing ->
          P.Do _ _ -> err "Unexpected 'do' statement with non-deterministic branches."

translateRange :: Int -> M.Map String String -> P.Range -> Err (String, P'.Exp, P'.Exp)
translateRange p venv = \case
  P.Between x e1 e2 -> do
    (e1', e2') <- binaryCons (translateExpPos p venv) (,) e1 e2
    return (x, e1', e2')
  _ -> Bad "Unexpected range over array."

translateExpPos :: Int -> M.Map String String -> P.Exp -> Err P'.Exp
translateExpPos p venv =
  let err = posErr p
      translateExp = translateExpPos p
      bin = binaryCons (translateExp venv)
      un = unaryCons (translateExp venv)
   in \case
        P.Const (P.VInt n) -> return $ P'.CNum n
        P.Const (P.VBool False) -> return P'.False
        P.Const (P.VBool True) -> return P'.True
        P.Not e -> un P'.Not e
        P.And e1 e2 -> bin P'.And e1 e2
        P.Or e1 e2 -> bin P'.Or e1 e2
        P.Le e1 e2 -> bin P'.Le e1 e2
        P.Ge e1 e2 -> bin P'.Ge e1 e2
        P.Lt e1 e2 -> bin P'.Lt e1 e2
        P.Gt e1 e2 -> bin P'.Gt e1 e2
        P.Eq e1 e2 -> bin P'.Eq e1 e2
        P.Ne e1 e2 -> bin P'.Ne e1 e2
        P.Plus e1 e2 -> bin P'.Plus e1 e2
        P.Minus e1 e2 -> bin P'.Minus e1 e2
        P.Mult e1 e2 -> bin P'.Mult e1 e2
        P.Div e1 e2 -> bin P'.Div e1 e2
        P.Neg e -> un P'.Neg e
        P.EVar (P.Var x) ->
          case M.lookup x venv of
            Just e' -> return $ P'.Var e'
            Nothing -> err $ "[INVALID VARIABLE] binding not found for: " ++ x
        e -> err $ "Promela to Go: Unexpected expression translation: " ++ show e

translateOp :: Ctxt (Pos P.Stmt) a -> Err (Ctxt () (Pos P'.Stmt))
translateOp ctx =
  let translate p cons c =
        case M.lookup c (chenv ctx) of
          Just c' -> wrapCtx $ ctx <: Pos p (P'.Atomic (cons c'))
          Nothing -> Bad $ "[INVALID CHANNEL] binding not found for: " ++ c
   in case syntax ctx of
        Pos p (P.Send (P.Var c) _) -> translate p P'.Send c
        Pos p (P.Recv (P.Var c) _) -> translate p P'.Recv c
        Pos p _ -> Bad (":" ++ show p ++ ": Unexpected statement.")

translateVal :: P.Val -> Maybe P'.Exp
translateVal v = case v of
  P.VInt n -> return $ P'.CNum n
  P.VBool True -> return P'.True
  P.VBool False -> return P'.False
  _ -> Nothing