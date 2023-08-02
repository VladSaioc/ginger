module Pipeline.Translation.GoToIR (getIR) where

import Data.Map qualified as M
import Go.Ast qualified as P
import IR.Ast
import Utilities.Err
import Utilities.General
import Utilities.Position
import Utilities.PrettyPrint (PrettyPrint (prettyPrint))
import Utilities.TransformationCtx

data Ctxt a b = Ctxt
  { syntax :: a,
    pid :: Int,
    nextpid :: Int,
    loopcounter :: Int,
    casecounter :: Int,
    varenv :: M.Map String 𝐸,
    chenv :: M.Map String String,
    procs :: M.Map Int 𝑆,
    chans :: M.Map String 𝐸,
    curr :: b
  }
  deriving (Eq, Ord, Read)

instance TransformCtx Ctxt where
  source = syntax
  updateSource ctx a = ctx {syntax = a}
  object = curr
  updateObject ctx a = ctx {curr = a}

getIR :: P.Prog -> Err 𝑃
getIR (P.Prog ss) =
  let ctx =
        Ctxt
          { syntax = ss,
            pid = 0,
            nextpid = 1,
            loopcounter = 0,
            casecounter = 0,
            varenv = M.empty,
            procs = M.empty,
            chans = M.empty,
            chenv = M.empty,
            curr = Skip
          }
   in do
        ctx' <- translateStatements ctx
        let chs = M.elems $ M.mapWithKey Chan (chans ctx')
        let ps = M.elems $ procs ctx'
        return $ 𝑃 chs ps

translateStatements :: Ctxt [Pos P.Stmt] 𝑆 -> Err (Ctxt () 𝑆)
translateStatements ρ = case syntax ρ of
  [] -> done (ρ {procs = M.insert (pid ρ) (curr ρ) (procs ρ)})
  Pos _ s : ss -> case s of
    P.Skip -> translateStatements (ss >: ρ)
    P.Return -> translateStatements ([] >: ρ)
    P.Break -> translateStatements ([] >: ρ)
    P.Decl x e -> do
      let venv = varenv ρ
      e' <- translateExp venv e
      let ρ₁ = ss >: ρ {varenv = M.insert x e' venv}
      translateStatements ρ₁
    P.If e ss1 ss2 -> do
      let venv = varenv ρ
      e' <- translateExp venv e
      ρ₁ <- translateStatements (ss1 >: ρ <: Skip)
      ρ₂ <- translateStatements (ss2 >: ρ₁ <: Skip)
      let ρ₃ = ss >: ρ₂ <: Seq (curr ρ) (If e' (curr ρ₁) (curr ρ₂))
      translateStatements ρ₃
    P.Atomic op -> do
      ρ' <- translateOp $ op >: ρ
      let op' = Atomic $ curr ρ'
      let stm = Seq (curr ρ) op'
      translateStatements (ss >: ρ' <: stm)
    P.Chan c e -> do
      e' <- translateExp (varenv ρ) e
      let ρ₁ =
            ρ
              { chans = M.insert c e' (chans ρ),
                chenv = M.insert c c (chenv ρ)
              }
      translateStatements (ss >: ρ₁)
    P.Go ss' -> do
      ρ₁ <-
        translateStatements
          Ctxt
            { syntax = ss',
              procs = procs ρ,
              pid = nextpid ρ,
              nextpid = nextpid ρ + 1,
              casecounter = casecounter ρ,
              loopcounter = loopcounter ρ,
              varenv = varenv ρ,
              chenv = chenv ρ,
              chans = chans ρ,
              curr = Skip
            }
      let ρ₂ =
            ρ
              { nextpid = nextpid ρ₁,
                loopcounter = loopcounter ρ₁,
                procs = procs ρ₁,
                chans = chans ρ₁,
                chenv = chenv ρ₁
              }
      translateStatements (ss >: ρ₂)
    P.For x e1 e2 diff ss' -> do
      let venv = varenv ρ
      (e1', e2') <- binaryCons (translateExp venv) (,) e1 e2
      ρ' <- translateFor (ss' >: ρ <: [])
      let for = case diff of
            P.Inc -> For (x ++ "'" ++ show (loopcounter ρ')) e1' e2' $ curr ρ'
            P.Dec -> For (x ++ "'" ++ show (loopcounter ρ')) e2' e1' $ curr ρ'
      let ρ'' = (ρ <: Seq (curr ρ) for) {loopcounter = loopcounter ρ' + 1}
      translateStatements $ ss >: ρ''
    P.Block ss' -> translateStatements $ (ss ++ ss') >: ρ
    P.Select cs Nothing -> do
      let -- Get channel operation case
          getChannelCase r cas@(Pos p o, _) = do
            c <- r
            case o of
              P.Star -> return c
              P.Send _ -> maybe (return $ return cas) (const $ posErr p "Multiple channel operations in 'select") c
              P.Recv _ -> maybe (return $ return cas) (const $ posErr p "Multiple channel operations in 'select") c
      c <- Prelude.foldl getChannelCase (return Nothing) cs
      ρ' <- case c of
        Just (Pos _ o, ss') -> do
          ρ₁ <- translateOp $ o >: ρ
          let o' = curr ρ₁
          ρ₂ <- translateStatements $ ss' >: ρ₁ <: Skip
          return $ ρ₂ <: Seq (Atomic o') (curr ρ₂)
        Nothing -> return $ () >: ρ <: Skip
      let translateSelect mρ (Pos _ o, s'') = do
            ρ₀ <- mρ
            case o of
              P.Star -> do
                ρ₁ <- translateStatements $ s'' >: ρ₀ <: Skip
                let guard = Var ("S'" ++ show (casecounter ρ₁))
                let select = If guard (curr ρ₁) (curr ρ₀)
                let ρ₂ = ρ₁ {casecounter = casecounter ρ₁ + 1}
                return $ ρ₂ <: select
              _ -> return ρ₀
      ρ₂ <- Prelude.foldl translateSelect (return ρ') cs
      let ρ₃ = ρ₂ <: Seq (curr ρ) (curr ρ₂)
      translateStatements $ ss >: ρ₃
    _ -> translateStatements $ ss >: ρ

translateFor :: Ctxt [Pos P.Stmt] [Op] -> Err (Ctxt () [Op])
translateFor ρ = case syntax ρ of
  [] -> done $ ρ <: reverse (curr ρ)
  Pos p s : ss -> case s of
    P.Atomic op -> do
      ρ₁ <- translateOp $ op >: ρ
      let ρ₂ = ρ₁ <: (curr ρ₁ : curr ρ)
      translateFor $ ss >: ρ₂
    P.Skip -> translateFor $ ss >: ρ
    _ -> posErr p $ "Unexpected statement: " ++ prettyPrint 0 s

translateExp :: M.Map String 𝐸 -> P.Exp -> Err 𝐸
translateExp venv =
  let bin = binaryCons (translateExp venv)
   in \case
        P.CTrue -> return BTrue
        P.CFalse -> return BFalse
        P.And e1 e2 -> bin (:&) e1 e2
        P.Or e1 e2 -> bin (:|) e1 e2
        P.Not e -> unaryCons (translateExp venv) Not e
        P.Neg e -> do
          e' <- translateExp venv e
          return $ Const 0 :- e'
        P.Eq e1 e2 -> bin (:==) e1 e2
        P.Ne e1 e2 -> bin (:!=) e1 e2
        P.Le e1 e2 -> bin (:<=) e1 e2
        P.Lt e1 e2 -> bin (:<) e1 e2
        P.Ge e1 e2 -> bin (:>=) e1 e2
        P.Gt e1 e2 -> bin (:>) e1 e2
        P.CNum n -> return $ Const n
        P.Plus e1 e2 -> bin (:+) e1 e2
        P.Minus e1 e2 -> bin (:-) e1 e2
        P.Mult e1 e2 -> bin (:*) e1 e2
        P.Div e1 e2 -> bin (:/) e1 e2
        P.Var x ->
          case M.lookup x venv of
            Just e' -> return e'
            Nothing -> return $ Var x

translateOp :: Ctxt P.CommOp a -> Err (Ctxt () Op)
translateOp ρ =
  let translate cons c =
        case M.lookup c (chenv ρ) of
          Just c' -> done (ρ <: cons c')
          Nothing -> Bad $ "Invalid channel: value not found: " ++ show c
   in case syntax ρ of
        P.Send c -> translate Send c
        P.Recv c -> translate Recv c
        s -> Bad $ "Unexpected communication operation: " ++ show s
