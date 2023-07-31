module Pipeline.Translation.GoToIR (getIR) where

import Data.Map qualified as M
import Go.Ast qualified as P
import IR.Ast qualified as T
import Utilities.Err
import Utilities.General
import Utilities.Position
import Utilities.PrettyPrint (PrettyPrint (prettyPrint))

data Ctxt a b = Ctxt
  { syntax :: a,
    pid :: Int,
    nextpid :: Int,
    loopcounter :: Int,
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

getIR :: P.Prog -> Err T.𝑃
getIR (P.Prog ss) =
  let ctx =
        Ctxt
          { syntax = ss,
            pid = 0,
            nextpid = 1,
            loopcounter = 0,
            varenv = M.empty,
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
  Pos _ s : ss -> case s of
    P.Skip -> translateStatements (ss >: ctx)
    P.Return -> translateStatements ([] >: ctx)
    P.Break -> translateStatements ([] >: ctx)
    P.If e ss1 ss2 -> do
      let venv = varenv ctx
      e' <- translateExp venv e
      ctx1 <- translateStatements (ss1 >: ctx <: T.Skip)
      ctx2 <- translateStatements (ss2 >: ctx1 <: T.Skip)
      let ctx3 = ss >: ctx2 <: T.If e' (curr ctx1) (curr ctx2)
      translateStatements ctx3
    P.Atomic op -> do
      ctx' <- translateOp $ op >: ctx
      let op' = T.Atomic $ curr ctx'
      let stm = T.Seq (curr ctx) op'
      translateStatements (ss >: ctx' <: stm)
    P.Chan c e -> do
      e' <- translateExp (varenv ctx) e
      let ctx1 =
            ctx
              { chans = M.insert c e' (chans ctx),
                chenv = M.insert c c (chenv ctx)
              }
      translateStatements (ss >: ctx1)
    P.Go ss' -> do
      ctx1 <-
        translateStatements
          Ctxt
            { syntax = ss',
              procs = procs ctx,
              pid = nextpid ctx,
              nextpid = nextpid ctx + 1,
              loopcounter = loopcounter ctx,
              varenv = varenv ctx,
              chenv = chenv ctx,
              chans = chans ctx,
              curr = T.Skip
            }
      let ctx2 =
            ctx
              { nextpid = nextpid ctx1,
                loopcounter = loopcounter ctx1,
                procs = procs ctx1,
                chans = chans ctx1,
                chenv = chenv ctx1
              }
      translateStatements (ss >: ctx2)
    P.For x e1 e2 diff ss' -> do
      let venv = varenv ctx
      (e1', e2') <- binaryCons (translateExp venv) (,) e1 e2
      ctx' <- translateFor (ss' >: ctx <: [])
      let for = case diff of
            P.Inc -> T.For (x ++ "'" ++ show (loopcounter ctx')) e1' e2' $ curr ctx'
            P.Dec -> T.For (x ++ "'" ++ show (loopcounter ctx')) e2' e1' $ curr ctx'
      let ctx'' = (ctx <: T.Seq (curr ctx) for) {loopcounter = loopcounter ctx' + 1}
      translateStatements $ ss >: ctx''
    P.Block ss' -> translateStatements $ (ss ++ ss') >: ctx
    _ -> translateStatements $ ss >: ctx

translateFor :: Ctxt [Pos P.Stmt] [T.Op] -> Err (Ctxt () [T.Op])
translateFor ctx = case syntax ctx of
  [] -> wrapCtx $ ctx <: reverse (curr ctx)
  Pos p s : ss -> case s of
    P.Atomic op -> do
      ctx' <- translateOp $ op >: ctx
      let ctx'' = ctx' <: (curr ctx' : curr ctx)
      translateFor $ ss >: ctx''
    P.Skip -> translateFor $ ss >: ctx
    _ -> posErr p $ "Unexpected statement: " ++ prettyPrint 0 s

translateExp :: M.Map String T.𝐸 -> P.Exp -> Err T.𝐸
translateExp venv =
  let bin = binaryCons (translateExp venv)
   in \case
        P.CNum n -> return $ T.Const n
        P.Plus e1 e2 -> bin (T.:+) e1 e2
        P.Minus e1 e2 -> bin (T.:-) e1 e2
        P.Mult e1 e2 -> bin (T.:*) e1 e2
        P.Div e1 e2 -> bin (T.:/) e1 e2
        P.Var x ->
          case M.lookup x venv of
            Just e' -> return e'
            Nothing -> return $ T.Var x
        _ -> Bad "Unexpected expression translation"

translateOp :: Ctxt P.CommOp a -> Err (Ctxt () T.Op)
translateOp ctx =
  let translate cons c =
        case M.lookup c (chenv ctx) of
          Just c' -> wrapCtx (ctx <: cons c')
          Nothing -> Bad $ "Invalid channel: value not found: " ++ show c
   in case syntax ctx of
        P.Send c -> translate T.Send c
        P.Recv c -> translate T.Recv c
        s -> Bad $ "Unexpected communication operation: " ++ show s
