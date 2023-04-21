module Pipeline.Translation.PromelaToIR where

import Data.Map qualified as M
import Data.Maybe
import IR.Ast qualified as P'
import Promela.Ast qualified as P
import Promela.Utilities
import Utilities.Err
import Utilities.Position

data Ctxt a b = Ctxt
  { syntax :: a,
    pid :: Int,
    varenv :: M.Map String String,
    procs :: M.Map Int P'.Stmt,
    chans :: M.Map String P'.Exp,
    curr :: b
  }
  deriving (Eq, Ord, Read)

wrapCtx :: Ctxt a b -> Err (Ctxt () b)
wrapCtx ctx = return ctx {syntax = ()}

($>) :: Int -> String -> String
($>) pid x = show pid ++ "'" ++ x

(<:) :: Ctxt a c -> b -> Ctxt a b
(<:) ctx b = ctx {curr = b}

(>:) :: a -> Ctxt c b -> Ctxt a b
(>:) a ctx = ctx {syntax = a}

($:) :: Ctxt a b -> Int -> P'.Stmt -> Ctxt a b
($:) ctx pid stm = ctx {procs = M.insert pid stm (procs ctx)}

getIR :: P.Spec -> Err P'.Prog
getIR (P.Spec ms) = case getInit (P.Spec ms) of
  Just i -> case i of
    P.Init ss ->
      let ctx =
            Ctxt
              { syntax = ss,
                pid = 0,
                varenv = M.empty,
                procs = M.empty,
                chans = M.empty,
                curr = P'.Skip
              }
       in Ok (P'.Prog [] [])
    _ -> Ok (P'.Prog [] [])
  Nothing -> Ok (P'.Prog [] [])

translateStatements :: Ctxt [Pos P.Stmt] P'.Stmt -> Err (Ctxt () P'.Stmt)
translateStatements ctx = case syntax ctx of
  [] ->
    let stm =
          case M.lookup (pid ctx) (procs ctx) of
            Just stm' -> P'.Seq stm' P'.Skip
            Nothing -> P'.Skip
        ctx' = (ctx $: pid ctx) stm
     in wrapCtx ctx'
  Pos p s : ss -> do
    let addOp op = do
          ctx' <- translateOp (Pos p op >: ctx)
          let stm = P'.Seq (curr ctx) (P'.Atomic (curr ctx'))
          translateStatements (ss >: ctx' <: stm)
    case s of
      P.Send {} -> addOp s
      P.Rcv {} -> addOp s
      _ -> wrapCtx ctx

translateFor :: Ctxt [Pos P.Stmt] [P'.Op] -> Err (Ctxt () [P'.Op])
translateFor ctx = case syntax ctx of
  [] -> wrapCtx (ctx {curr = reverse (curr ctx)})
  Pos p s : ss -> do
    let err msg = Bad (":" ++ show p ++ ": " ++ msg)
    let addOp op = do
          ctx' <- translateOp (Pos p op >: ctx)
          return (ctx' <: (curr ctx' : curr ctx))
    ctx' <-
      ( case s of
          P.Rcv {} -> addOp s
          P.Send {} -> addOp s
          _ -> err "Unexpected statement"
        )
    translateFor (ctx' {syntax = ss})

translateOp :: Ctxt (Pos P.Stmt) a -> Err (Ctxt () P'.Op)
translateOp ctx =
  let translate cons c = do
        c' <- translateVar (c >: ctx)
        wrapCtx (ctx <: cons c')
   in case syntax ctx of
        Pos _ (P.Send c _) -> translate P'.Send c
        Pos _ (P.Rcv c _) -> translate P'.Recv c
        Pos p _ -> Bad (":" ++ show p ++ ": Unexpected statement.")

translateVar :: Ctxt P.LVal a -> Err String
translateVar ctx = case syntax ctx of
  P.Var x -> case M.lookup x (varenv ctx) of
    Just x' -> Ok x'
    Nothing -> Bad ("Found in-use Promela variable without equivalent source binding: " ++ x)
  _ -> Bad "Invalid variable translation: found non-variable ℓ-value"
