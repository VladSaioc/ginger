module Pipeline.Translation.AlphaConversion (alphaConvert) where

import Data.Map as M
import Data.Maybe
import Promela.Ast
import Utilities.Position

-- Environment from original source name to alpha-converted name
type Env = M.Map String String

-- Environment from original source name to alpha-index
type AEnv = M.Map String Int

-- Alpha conversion context:
-- 1. The binding in the current scope
-- 2. The next available name
data Ctxt a = Ctxt Env AEnv a deriving (Eq, Ord, Read, Show)

alphaConvert :: Spec -> Spec
alphaConvert (Spec ms) =
  let ctx' = Prelude.foldl alphaDefs (Ctxt M.empty M.empty []) ms
      Ctxt _ _ ms' = Prelude.foldl alphaModules ctx' ms
   in Spec (reverse ms')

alphaDeclareVar :: (Env, AEnv) -> Ident -> (Env, AEnv)
alphaDeclareVar (env, aenv) x =
  let i = Data.Maybe.fromMaybe 0 (M.lookup x aenv)
      x' = x ++ show i
      aenv' = M.insert x (i + 1) aenv
      env' = M.insert x x' env
   in (env', aenv')

alphaDefs :: Ctxt [Module] -> Module -> Ctxt [Module]
alphaDefs ctx@(Ctxt env aenv ms) = \case
  TopDecl x t v ->
    let (env', aenv') = alphaDeclareVar (env, aenv) x
        x' = Data.Maybe.fromJust (M.lookup x env')
     in Ctxt env' aenv' (TopDecl x' t v : ms)
  _ -> ctx

alphaModules :: Ctxt [Module] -> Module -> Ctxt [Module]
alphaModules ctx@(Ctxt env aenv ms) = \case
  TopDecl {} -> ctx
  Init ss ->
    let Ctxt _ aenv' ss' = Prelude.foldl alphaStmt (Ctxt env aenv []) ss
     in Ctxt env aenv' (Init (reverse ss') : ms)
  Proc f ps ss ->
    let Ctxt env' aenv' ps' = Prelude.foldl alphaParam (Ctxt env aenv []) ps
        Ctxt _ aenv'' ss' = Prelude.foldl alphaStmt (Ctxt env' aenv' []) ss
     in Ctxt env aenv'' (Proc f (reverse ps') (reverse ss') : ms)
  Typedef t fs ->
    let fs' = Prelude.map (\(f, ft, me) -> (f, ft, fmap (alphaExp env) me)) fs
     in Ctxt env aenv (Typedef t fs' : ms)

alphaParam :: Ctxt [Param] -> Param -> Ctxt [Param]
alphaParam (Ctxt env aenv ps) (x, t) =
  let (env', aenv') = alphaDeclareVar (env, aenv) x
      x' = Data.Maybe.fromJust (M.lookup x env')
   in Ctxt env' aenv' ((x', t) : ps)

alphaStmt :: Ctxt [Pos Stmt] -> Pos Stmt -> Ctxt [Pos Stmt]
alphaStmt ctx@(Ctxt env aenv ss) (Pos p s) =
  let alph = Prelude.foldl alphaStmt
      controlFlow c os mels =
        let alpha ctx' ss'' =
              let Ctxt e a ss' = alphaStmt ctx' ss''
               in Ctxt e a (reverse ss')
            alphaBranch (Ctxt e a bs) b =
              let Ctxt e' a' b' = alpha (Ctxt e a []) b
               in Ctxt e' a' (b' : bs)
            Ctxt env' aenv' os' = Prelude.foldl (Prelude.foldl alphaBranch) (Ctxt env aenv []) os
            Ctxt env'' aenv'' mels' = case mels of
              Just els ->
                let Ctxt aenv''' env''' els' = alph (Ctxt env' aenv' []) els
                 in Ctxt aenv''' env''' (Just els')
              Nothing -> Ctxt env' aenv' Nothing
         in Ctxt env'' aenv'' (p @ c (reverse os') mels' : ss)
      chOp c x es =
        let x' = alphaLVal env x
            es' = Prelude.map (alphaExp env) es
         in Ctxt env aenv (p @ c x' es' : ss)
   in case s of
        Decl x t e ->
          let e' = fmap (alphaExp env) e
              (env', aenv') = alphaDeclareVar (env, aenv) x
              x' = Data.Maybe.fromJust (M.lookup x env')
              s' = Pos p (Decl x' t e')
           in Ctxt env' aenv' (s' : ss)
        If os mels -> controlFlow If os mels
        Do os mels -> controlFlow Do os mels
        For r body ->
          let r' = alphaRange env r
              Ctxt _ aenv' body' = alph (Ctxt env aenv []) body
           in Ctxt env aenv' (p @ For r' body' : ss)
        As x e ->
          let x' = alphaLVal env x
              e' = alphaExp env e
           in Ctxt env aenv (p @ As x' e' : ss)
        Goto lbl -> Ctxt env aenv (p @ Goto lbl : ss)
        Break -> Ctxt env aenv (p @ Break : ss)
        Skip -> Ctxt env aenv (p @ Skip : ss)
        Assert e ->
          let e' = alphaExp env e
           in Ctxt env aenv (p @ Assert e' : ss)
        Recv c es -> chOp Recv c es
        Send c es -> chOp Send c es
        ExpS e ->
          let e' = alphaExp env e
           in Ctxt env aenv (p @ ExpS e' : ss)
        Label lbl stm ->
          let Ctxt env' aenv' [stm'] = alph ctx [stm]
           in Ctxt env' aenv' (p @ Label lbl stm' : ss)

alphaExp :: Env -> Exp -> Exp
alphaExp env =
  let alpha = alphaExp env
      bin c e1 e2 = c (alpha e1) (alpha e2)
   in \case
        Chan e -> Chan (alpha e)
        Const v -> Const v
        And e1 e2 -> bin And e1 e2
        Or e1 e2 -> bin Or e1 e2
        Eq e1 e2 -> bin Eq e1 e2
        Ne e1 e2 -> bin Ne e1 e2
        Le e1 e2 -> bin Le e1 e2
        Lt e1 e2 -> bin Lt e1 e2
        Ge e1 e2 -> bin Ge e1 e2
        Gt e1 e2 -> bin Gt e1 e2
        Plus e1 e2 -> bin Plus e1 e2
        Minus e1 e2 -> bin Minus e1 e2
        Mult e1 e2 -> bin Mult e1 e2
        Div e1 e2 -> bin Div e1 e2
        Neg e -> Neg (alpha e)
        Not e -> Not (alpha e)
        Len x -> Len (alphaLVal env x)
        EVar x -> EVar (alphaLVal env x)
        Run f es -> Run f (Prelude.map alpha es)

alphaLVal :: Env -> LVal -> LVal
alphaLVal env = \case
  Var x ->
    let x' = Data.Maybe.fromJust (M.lookup x env)
     in Var x'
  Field x f -> Field (alphaLVal env x) f
  Arr x e -> Arr (alphaLVal env x) (alphaExp env e)

alphaRange :: Env -> Range -> Range
alphaRange env = \case
  Between x e1 e2 ->
    let x' = Data.Maybe.fromJust (M.lookup x env)
     in Between x' (alphaExp env e1) (alphaExp env e2)
  In x1 x2 ->
    let x1' = Data.Maybe.fromJust (M.lookup x1 env)
        x2' = Data.Maybe.fromJust (M.lookup x2 env)
     in In x1' x2'
