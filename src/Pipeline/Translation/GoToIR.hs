module Pipeline.Translation.GoToIR (getIR) where

import Data.Map qualified as M
import Go.Ast qualified as P
import IR.Ast qualified as P'
import Utilities.Err
import Utilities.General
import Utilities.Position
import Utilities.PrettyPrint (PrettyPrint (prettyPrint))

data Ctxt a b = Ctxt
  { syntax :: a,
    pid :: Int,
    nextpid :: Int,
    loopcounter :: Int,
    varenv :: M.Map String P'.Exp,
    chenv :: M.Map String String,
    procs :: M.Map Int P'.Stmt,
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

getIR :: P.Prog -> Err P'.Prog
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
            curr = P'.Skip
          }
   in do
        ctx' <- translateStatements ctx
        let chs = M.elems $ M.mapWithKey P'.Chan (chans ctx')
        let ps = M.elems $ procs ctx'
        return $ P'.Prog chs ps

translateStatements :: Ctxt [Pos P.Stmt] P'.Stmt -> Err (Ctxt () P'.Stmt)
translateStatements ctx = case syntax ctx of
  [] -> wrapCtx (ctx {procs = M.insert (pid ctx) (curr ctx) (procs ctx)})
  Pos _ s : ss -> case s of
    P.Atomic op -> do
      ctx' <- translateOp $ op >: ctx
      let op' = P'.Atomic $ curr ctx'
      let stm = P'.Seq (curr ctx) op'
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
              curr = P'.Skip
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
            P.Inc -> P'.For (x ++ "'" ++ show (loopcounter ctx')) e1' e2' $ curr ctx'
            P.Dec -> P'.For (x ++ "'" ++ show (loopcounter ctx')) e2' e1' $ curr ctx'
      let ctx'' = (ctx <: P'.Seq (curr ctx) for) {loopcounter = loopcounter ctx' + 1}
      translateStatements $ ss >: ctx''
    P.Block ss' -> translateStatements $ (ss ++ ss') >: ctx
    _ -> translateStatements $ ss >: ctx

translateFor :: Ctxt [Pos P.Stmt] [P'.Op] -> Err (Ctxt () [P'.Op])
translateFor ctx = case syntax ctx of
  [] -> wrapCtx $ ctx <: reverse (curr ctx)
  Pos p s : ss -> case s of
    P.Atomic op -> do
      ctx' <- translateOp $ op >: ctx
      let ctx'' = ctx' <: (curr ctx' : curr ctx)
      translateFor $ ss >: ctx''
    P.Skip -> translateFor $ ss >: ctx
    _ -> posErr p $ "Unexpected statement: " ++ prettyPrint 0 s

translateExp :: M.Map String P'.Exp -> P.Exp -> Err P'.Exp
translateExp venv =
  let bin = binaryCons (translateExp venv)
   in \case
        P.CNum n -> return $ P'.Const n
        P.Plus e1 e2 -> bin P'.Plus e1 e2
        P.Minus e1 e2 -> bin P'.Minus e1 e2
        P.Mult e1 e2 -> bin P'.Mult e1 e2
        P.Div e1 e2 -> bin P'.Div e1 e2
        P.Var x ->
          case M.lookup x venv of
            Just e' -> return e'
            Nothing -> return $ P'.Var x
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
