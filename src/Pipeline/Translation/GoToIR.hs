module Pipeline.Translation.GoToIR (getIR) where

import Data.Map qualified as M
import Go.Ast qualified as P
import IR.Ast qualified as P'
import Utilities.Err
import Utilities.General
import Utilities.PrettyPrint (PrettyPrint (prettyPrint))

data Ctxt a b = Ctxt
  { syntax :: a,
    pid :: Int,
    nextpid :: Int,
    varenv :: M.Map String P'.Exp,
    chenv :: M.Map String String,
    procs :: M.Map Int P'.Stmt,
    chans :: M.Map String P'.Exp,
    curr :: b
  }
  deriving (Eq, Ord, Read)

wrapCtx :: Ctxt a b -> Err (Ctxt () b)
wrapCtx ctx = return ctx {syntax = ()}

(<:) :: Ctxt a c -> b -> Ctxt a b
(<:) ctx b = ctx {curr = b}

(>:) :: a -> Ctxt c b -> Ctxt a b
(>:) a ctx = ctx {syntax = a}

getIR :: P.Prog -> Err P'.Prog
getIR p@(P.Prog ss) =
  let ctx =
        Ctxt
          { syntax = ss,
            pid = 0,
            nextpid = 1,
            varenv = M.empty,
            procs = M.empty,
            chans = M.empty,
            chenv = M.empty,
            curr = P'.Skip
          }
   in do
        ctx' <- translateStatements ctx
        let chs = M.elems $ M.mapWithKey P'.Chan (chans ctx')
        let ps = M.elems $ procs ctx'
        return $ P'.Prog chs ps

translateStatements :: Ctxt [P.Stmt] P'.Stmt -> Err (Ctxt () P'.Stmt)
translateStatements ctx = case syntax ctx of
  [] -> wrapCtx (ctx {procs = M.insert (pid ctx) (curr ctx) (procs ctx)})
  s : ss -> do
    let addOp op = do
          ctx' <- translateOp $ op >: ctx
          let stm = P'.Seq (curr ctx) (P'.Atomic (curr ctx'))
          translateStatements (ss >: ctx' <: stm)
    case s of
      P.Atomic op -> addOp op
      P.Chan c e -> do
        e' <- translateExp (varenv ctx) e
        let ctx1 =
              ctx
                { chans = M.insert c e' (chans ctx),
                  chenv = M.insert c c (chenv ctx)
                }
        translateStatements (ss >: ctx1)
      P.Go ss' -> do
        let ctx1 =
              Ctxt
                { syntax = ss',
                  pid = nextpid ctx,
                  nextpid = nextpid ctx + 1,
                  varenv = M.empty,
                  chenv = chenv ctx,
                  procs = procs ctx,
                  chans = chans ctx,
                  curr = P'.Skip
                }
        ctx2 <- translateStatements ctx1
        let ctx3 =
              ctx
                { nextpid = nextpid ctx2,
                  procs = procs ctx2,
                  chans = chans ctx2
                }
        translateStatements (ss >: ctx3)
      P.For x e1 e2 diff ss' -> do
        let venv = varenv ctx
        (e1', e2') <- binaryCons (translateExp venv) (,) e1 e2
        ctx' <- translateFor (ss' >: ctx <: [])
        let for = case diff of
              P.Inc -> P'.For x e1' e2' $ curr ctx'
              P.Dec -> P'.For x e2' e1' $ curr ctx'
        let ctx'' = ctx <: P'.Seq (curr ctx) for
        translateStatements $ ss >: ctx''
      _ -> translateStatements (ctx {syntax = ss})

translateFor :: Ctxt [P.Stmt] [P'.Op] -> Err (Ctxt () [P'.Op])
translateFor ctx = case syntax ctx of
  [] -> wrapCtx (ctx {curr = reverse (curr ctx)})
  P.Atomic op : ss -> do
    let addOp op = do
          ctx' <- translateOp $ op >: ctx
          return (ctx' <: (curr ctx' : curr ctx))
    ctx' <- addOp op
    translateFor $ ss >: ctx'
  P.Skip : ss -> translateFor $ ss >: ctx
  s : _ -> Bad $ "Unexpected statement: " ++ prettyPrint 0 s

translateExp :: M.Map String P'.Exp -> P.Exp -> Err P'.Exp
translateExp venv =
  let bin = binaryCons (translateExp venv)
   in \case
        P.CNum n -> return (P'.Const n)
        P.Plus e1 e2 -> bin P'.Plus e1 e2
        P.Minus e1 e2 -> bin P'.Minus e1 e2
        P.Mult e1 e2 -> bin P'.Mult e1 e2
        P.Div e1 e2 -> bin P'.Div e1 e2
        P.Var x ->
          case M.lookup x venv of
            Just e' -> return e'
            Nothing -> Bad ("Unrecognized variable: " ++ x)
        _ -> Bad "Unexpected expression translation"

translateOp :: Ctxt P.CommOp a -> Err (Ctxt () P'.Op)
translateOp ctx =
  let translate cons c =
        case M.lookup c (chenv ctx) of
          Just c' -> wrapCtx (ctx <: cons c')
          Nothing -> Bad "Invalid channel: value not found."
   in case syntax ctx of
        P.Send c -> translate P'.Send c
        P.Recv c -> translate P'.Recv c
        s -> Bad $ "Unexpected communication operation: " ++ show s
