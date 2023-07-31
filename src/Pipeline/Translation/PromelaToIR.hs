module Pipeline.Translation.PromelaToIR (getIR) where

import Data.Map qualified as M
import IR.Ast qualified as T
import Pipeline.Callgraph (getCG)
import Promela.Ast qualified as P
import Promela.Utilities
import Utilities.Err
import Utilities.General
import Utilities.Position

data Ctxt a b = Ctxt
  { syntax :: a,
    pid :: Int,
    nextpid :: Int,
    cg :: M.Map String P.Module,
    varenv :: M.Map String T.𝐸,
    chenv :: M.Map String String,
    procs :: M.Map Int T.𝑆,
    chans :: M.Map String T.𝐸,
    curr :: b
  }
  deriving (Eq, Ord, Read)

wrapCtx :: Ctxt a b -> Err (Ctxt () b)
wrapCtx ctx = return ctx {syntax = ()}

(<:) :: Ctxt a c -> b -> Ctxt a b
(<:) ctx b = ctx {curr = b}

(>:) :: a -> Ctxt c b -> Ctxt a b
(>:) a ctx = ctx {syntax = a}

getIR :: P.Spec -> Err T.𝑃
getIR p@(P.Spec ms) =
  let getFV venv = \case
        P.TopDecl x P.TInt v -> M.insert x (translateVal x v) venv
        _ -> venv
      ctx =
        Ctxt
          { syntax = getInit p,
            pid = 0,
            nextpid = 1,
            cg = getCG p,
            varenv = Prelude.foldl getFV M.empty ms,
            procs = M.empty,
            chans = M.empty,
            chenv = M.empty,
            curr = T.Skip
          }
   in do
        ctx' <- translateStatements ctx
        let chs = M.elems $ M.mapWithKey T.Chan (chans ctx')
        let ps = M.elems $ procs ctx'
        return $ T.𝑃 chs ps

translateStatements :: Ctxt [Pos P.Stmt] T.𝑆 -> Err (Ctxt () T.𝑆)
translateStatements ctx = case syntax ctx of
  [] -> wrapCtx (ctx {procs = M.insert (pid ctx) (curr ctx) (procs ctx)})
  Pos p s : ss -> do
    let addOp op = do
          ctx' <- translateOp (Pos p op >: ctx)
          let stm = T.Seq (curr ctx) (T.Atomic (curr ctx'))
          translateStatements (ss >: ctx' <: stm)
        err = posErr p
    case s of
      P.Send {} -> addOp s
      P.Recv {} -> addOp s
      P.Decl c P.TChan me ->
        case me of
          Just (P.Chan e) -> do
            e' <- translateExp (varenv ctx) e
            let ctx1 =
                  ctx
                    { chans = M.insert c e' (chans ctx),
                      chenv = M.insert c c (chenv ctx)
                    }
            translateStatements (ss >: ctx1)
          _ -> err $ "Channel " ++ c ++ " has no capacity."
      P.ExpS (P.Run f es) ->
        case M.lookup f (cg ctx) of
          Just (P.Proc _ ps ss') -> do
            let pes = zip ps es
                addVar ve ((x, t), e) =
                  case t of
                    P.TInt -> case translateExp (varenv ctx) e of
                      Ok e' -> M.insert x e' ve
                      _ -> ve
                    _ -> ve
            let addCh ce ((a, t), e) =
                  case (t, e) of
                    (P.TChan, P.EVar (P.Var c)) -> M.insert a c ce
                    _ -> ce
            let ctx1 =
                  Ctxt
                    { syntax = ss',
                      pid = nextpid ctx,
                      nextpid = nextpid ctx + 1,
                      cg = cg ctx,
                      varenv = Prelude.foldl addVar (varenv ctx) pes,
                      chenv = Prelude.foldl addCh M.empty pes,
                      procs = procs ctx,
                      chans = chans ctx,
                      curr = T.Skip
                    }
            ctx2 <- translateStatements ctx1
            let ctx3 =
                  ctx
                    { nextpid = nextpid ctx2,
                      procs = procs ctx2,
                      chans = chans ctx2
                    }
            translateStatements (ss >: ctx3)
          _ -> err $ "Function " ++ f ++ " not in call-graph."
      P.For r ss' -> do
        (x, e1', e2') <- translateRange (varenv ctx) r
        ctx' <- translateFor (ss' >: ctx <: [])
        let ctx'' = ctx <: T.Seq (curr ctx) (T.For x e1' e2' (curr ctx'))
        translateStatements (ss >: ctx'')
      _ -> translateStatements (ss >: ctx)

translateFor :: Ctxt [Pos P.Stmt] [T.Op] -> Err (Ctxt () [T.Op])
translateFor ctx = case syntax ctx of
  [] -> wrapCtx (ctx {curr = reverse (curr ctx)})
  Pos p s : ss -> do
    let err = posErr p
    let addOp op = do
          ctx' <- translateOp (Pos p op >: ctx)
          return (ctx' <: (curr ctx' : curr ctx))
    ctx' <-
      if commStmt s
        then addOp s
        else case s of
          P.Label _ -> wrapCtx ctx
          P.Assert _ -> wrapCtx ctx
          P.Skip -> wrapCtx ctx
          _ -> err ("Unexpected statement in for: " ++ show s)
    translateFor (ctx' {syntax = ss})

translateRange :: M.Map String T.𝐸 -> P.Range -> Err (String, T.𝐸, T.𝐸)
translateRange venv = \case
  P.Between x e1 e2 -> do
    (e1', e2') <- binaryCons (translateExp venv) (,) e1 e2
    return (x, e1', e2')
  _ -> Bad "Unexpected range over array."

translateExp :: M.Map String T.𝐸 -> P.Exp -> Err T.𝐸
translateExp venv =
  let bin = binaryCons (translateExp venv)
   in \case
        P.Const (P.VInt n) -> return (T.Const n)
        P.And e1 e2 -> bin (T.:&) e1 e2
        P.Or e1 e2 -> bin (T.:|) e1 e2
        P.Plus e1 e2 -> bin (T.:+) e1 e2
        P.Minus e1 e2 -> bin (T.:-) e1 e2
        P.Mult e1 e2 -> bin (T.:*) e1 e2
        P.Div e1 e2 -> bin (T.:/) e1 e2
        P.EVar (P.Var x) ->
          case M.lookup x venv of
            Just e' -> return e'
            Nothing -> Bad ("Unrecognized variable: " ++ x)
        _ -> Bad "Unexpected expression translation"

translateOp :: Ctxt (Pos P.Stmt) a -> Err (Ctxt () T.Op)
translateOp ctx =
  let translate cons c =
        case M.lookup c (chenv ctx) of
          Just c' -> wrapCtx (ctx <: cons c')
          Nothing -> Bad "Invalid channel: value not found."
   in case syntax ctx of
        Pos _ (P.Send (P.Var c) _) -> translate T.Send c
        Pos _ (P.Recv (P.Var c) _) -> translate T.Recv c
        Pos p s -> posErr p $ "Unexpected statement: " ++ show s

translateVal :: String -> P.Val -> T.𝐸
translateVal x = \case
  P.VInt n -> T.Const n
  _ -> T.Var x