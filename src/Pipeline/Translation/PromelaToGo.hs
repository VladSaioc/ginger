module Pipeline.Translation.PromelaToGo (getGo) where

import Control.Monad
import Data.List qualified as L
import Data.Map qualified as M
import Data.Maybe qualified as Mb
import Debug.Trace (trace)
import Go.Ast qualified as P'
import Go.Utilities (flipIfs)
import Pipeline.Callgraph (getCG)
import Promela.Ast qualified as P
import Promela.Utilities
import Utilities.Err
import Utilities.General (binaryCons, foldMonad, unaryCons)
import Utilities.Position
import Utilities.TransformationCtx

-- Promela-to-Go translation context
data Ctxt a b = Ctxt
  { -- Syntax to be translated
    syntax :: a,
    -- Promela call graph
    cg :: M.Map String P.Module,
    -- Prefix for local variables during intra-procedural translation
    prefix :: String,
    -- Variable environment.
    -- Binds Promela names to equivalently scoped Go names
    varenv :: M.Map String String,
    -- Counter number of calls to differentiate expansions
    -- of the same function at different call sites.
    calls :: Int,
    -- Channel name environment.
    -- Binds Promela channel names to equivalently scoped Go names
    chenv :: M.Map String String,
    -- Channel capacity environment.
    -- Binds Go channel names to Go capacity expressions.
    κ :: M.Map String P'.Exp,
    -- Current object syntax translation tracker
    curr :: b
  }
  deriving (Eq, Ord, Read, Show)

instance TransformCtx Ctxt where
  source = syntax
  updateSource ctx a = ctx {syntax = a}
  object = curr
  updateObject ctx a = ctx {curr = a}

-- Object syntax wrapper. Helps with hoisting declarations
-- before statements.
data Obj a b = Obj
  { decls :: a,
    stmts :: b
  }

type Go = Obj [Pos P'.Stmt] [Pos P'.Stmt]

-- Reconstruct a Go AST from the Promela encoding.
-- Explode program by following call edges.
--
-- IMPORTANT: Assumes alpha conversion and absence of recursion.
getGo :: P.Spec -> Err P'.Prog
getGo p@(P.Spec ms) =
  let -- Construct the initial variable name environment
      getEnv venv = \case
        -- Top-level variables are bound to their own names.
        P.TopDecl x _ _ -> M.insert x x venv
        -- Skip other module declarations
        _ -> venv
      -- Construct the initial variable value environment.
      -- Use it for initial variable declarations.
      getInitVals decls = \case
        -- Top-level variable declarations will get assigned
        -- their RHS in the top-level declaration, if specified
        P.TopDecl x _ v -> case translateVal v of
          -- If the value is an identifiable constant, create
          -- an assignment statement.
          Just v' -> Pos 0 (P'.Decl x v') : decls
          -- Otherwise, the variable is considered "free", so
          -- no initial assignment is made.
          Nothing -> decls
        _ -> decls
   in do
        -- Get initial declaration statements for top-level
        -- declarations.
        let initStmts = Prelude.foldl getInitVals [] ms
        -- Translation context
        let ctx =
              Ctxt
                { -- Start translation from initial process
                  syntax = getInit p,
                  -- Construct call-graph
                  cg = getCG p,
                  -- Initial process local variables do not need a prefix
                  prefix = "",
                  -- Initial variable environment includes all top-level declarations
                  varenv = Prelude.foldl getEnv M.empty ms,
                  -- Capacity and variable name environments are initially empty
                  κ = M.empty,
                  chenv = M.empty,
                  -- No calls have yet been executed
                  calls = 0,
                  -- The initial translation object only includes the initialization
                  -- statements for top-level declarations.
                  curr = Obj {stmts = [], decls = initStmts}
                }
        ctx' <- translateStatements ctx
        let Obj {decls, stmts} = curr ctx'
        return $ P'.Prog (decls ++ stmts)

translateStatements :: Ctxt [Pos P.Stmt] Go -> Err (Ctxt () Go)
translateStatements ρ = case syntax ρ of
  -- Produce object syntax.
  -- ! Statements reversed here to preserve linearity
  -- ! in the complexity of construction.
  [] ->
    let Obj {decls, stmts} = curr ρ
     in done $ ρ <: Obj {decls = decls, stmts = reverse stmts}
  Pos p s : ss ->
    let freshObj = Obj {decls = [], stmts = []}
        translateExp = translateExpPos p
        err = posErr p
        addOp op = do
          -- Translate channel operation
          ρ' <- translateOp (Pos p op >: ρ)
          let stm = curr ρ'
          let oss = stmts $ curr ρ
          -- Add statement to object syntax.
          let ρ₂ = ss >: ρ' <: (curr ρ) {stmts = stm : oss}
          translateStatements ρ₂
     in case s of
          -- Translation of assignment statements.
          -- Only assignments to plain variables are allowed.
          P.As (P.Var x) e -> do
            x' <-
              ( case M.lookup x (varenv ρ) of
                  Just x' -> return x'
                  Nothing -> err $ "[INVALID VARIABLE] binding not found for: " ++ x
                )
            e' <- translateExp (varenv ρ) e
            let Obj {stmts = oss} = curr ρ
            let oss' = Pos p (P'.As x' e') : oss
            let obj = (curr ρ) {stmts = oss'}
            translateStatements (ss >: ρ <: obj)
          P.As _ _ -> err "[INVALID ASSIGNMENT] unrecognized write to complex data structure"
          -- Channel operations
          P.Send {} -> addOp s
          P.Recv {} -> addOp s
          -- Assert statements are irrelevant
          P.Assert _ -> translateStatements (ss >: ρ)
          -- Skip statements are irrelevant
          P.Skip -> translateStatements (ss >: ρ)
          -- 'label:' statements are irrelevant
          P.Label _ -> translateStatements (ss >: ρ)
          -- Can discard the continuation of 'break', since it is unreachable.
          P.Break -> do
            let oss = stmts $ curr ρ
            let oss' = Pos p P'.Break : oss
            let obj = (curr ρ) {stmts = oss'}
            translateStatements ([] >: ρ <: obj)
          -- Can discard the continuation of 'goto stop_process', since it is unreachable.
          P.Goto "stop_process" -> do
            let oss = stmts $ curr ρ
            let oss' = Pos p P'.Return : oss
            let obj = (curr ρ) {stmts = oss'}
            translateStatements ([] >: ρ <: obj)
          P.Goto l -> err $ "Promela-to-Go: Unexpected statement: goto " ++ l
          -- Reduce Gomela for statements non-determinstic wrapping
          -- to underlying for statement:
          --
          -- if :: _ -> for (i : e1 .. e2) { S }; ...
          --    :: _ -> do :: _ -> S; ...
          -- fi
          P.If
            ((_, for@((Pos _ (P.For _ _)) : _)) : _)
            (Just _) -> translateStatements ((for ++ ss) >: ρ)
          -- General if statement translation
          P.If os mels' ->
            let notSelectMessage = "if is not select"
                -- First try to check whether the 'if' statement models a Go select statement
                makeSelect ρ' = case curr ρ' of
                  -- These represent the select statement so far.
                  -- 'cs' are the existing cases
                  -- 'def' is the optional default case
                  (ods, cs, def) ->
                    -- Add a communicating case
                    let addCommCase op c ss' = do
                          c' <- maybe (err ("Channel name not found: " ++ show c)) return $ M.lookup c (chenv ρ')
                          -- Translate the statements in the case body.
                          ctx'' <- translateStatements (ss' >: ρ' <: freshObj)
                          let Obj {stmts, decls = ods'} = curr ctx''
                          -- Add the translated case to the select statement.
                          let select = (ods ++ ods', (Pos p $ op c', stmts) : cs, def)
                          done $ ctx'' <: select
                     in \case
                          -- The 'default -> ...' branch discovered is considered
                          -- a 'default' case.
                          (Pos _ (P.ExpS (P.EVar (P.Var "default"))), ss') -> do
                            -- If more than one 'default' case exists,
                            -- then it's not a select statement.
                            ρ'' <- case def of
                              Nothing -> translateStatements (ss' >: ρ' <: freshObj)
                              Just _ -> Bad notSelectMessage
                            let Obj {stmts, decls = ods'} = curr ρ''
                            let select = (ods ++ ods', cs, Just stmts)
                            done $ ρ'' <: select
                          -- Cases of the form: 'c!_ -> ...'.
                          (Pos _ (P.Send (P.Var c) _), ss') -> addCommCase P'.Send c ss'
                          -- Cases of the form: 'c?_ -> ...'.
                          (Pos _ (P.Recv (P.Var c) _), ss') -> addCommCase P'.Recv c ss'
                          -- Cases of the form: 'true -> ...' are considered
                          -- operations on always potentially enabled channels.
                          -- This applies to timeouts or context channels.
                          (Pos p' (P.ExpS (P.Const (P.VBool True))), ss') -> do
                            ρ'' <- translateStatements (ss' >: ρ' <: freshObj)
                            let caseClause = Pos p' P'.Star
                            let Obj {stmts, decls = ods'} = curr ρ''
                            let select = (ods ++ ods', (caseClause, stmts) : cs, def)
                            done $ ρ'' <: select
                          -- Cases of the form 'c[_]!_ -> ...' or 'x.c!_ -> ...' are not
                          -- covered features.
                          (Pos _ (P.Send _ _), _) -> Bad "[INVALID SEND]: Operations on channel in aggregate data structures are not supported"
                          -- Cases of the form 'c[_]?_ -> ...' or 'x.c?_ -> ...' are not
                          -- covered features.
                          (Pos _ (P.Recv _ _), _) -> Bad "[INVALID RECEIVE]: Operations on channel in aggregate data structures are not supported"
                          _ -> Bad notSelectMessage
                -- If the 'if' is not considered viable to model a select statement,
                -- produce a regular 'if' statement instead.
                makeIf ρ1@Ctxt {curr = (ods, ifSoFar), varenv} = \case
                  (Pos p' (P.ExpS e), ss') -> do
                    e' <- translateExpPos p' varenv e
                    ρ2 <- translateStatements (ss' >: ρ1 <: freshObj)
                    let Obj {stmts = ss'', decls = ods'} = curr ρ2
                    let body = P'.If e' ss'' [Pos p' ifSoFar]
                    let obj' = (ods ++ ods', body)
                    done $ ρ2 <: obj'
                  -- Cases of the form 'c[_]!_ -> ...' or 'x.c!_ -> ...' are not
                  -- covered features.
                  (Pos _ (P.Send _ _), _) -> Bad "[INVALID SEND]: Operations on channel in perceived 'if'-statement"
                  -- Cases of the form 'c[_]?_ -> ...' or 'x.c?_ -> ...' are not
                  -- covered features.
                  (Pos _ (P.Recv _ _), _) -> Bad "[INVALID RECEIVE]: Operations on channel in perceived 'if'-statement"
                  _ -> Bad "[INVALID IF BRANCH]: If statement has unrecognizable branch."
             in case foldM makeSelect (() >: ρ <: ([], [], Nothing)) os of
                  Bad msg -> do
                    let mels = trace (show mels') mels'
                    -- If constructing a select failed because the if statement does
                    -- not model one, attempt to build a regular if statement.
                    _ <- if msg == notSelectMessage then return () else err msg
                    -- Translate the 'else' case, if present.
                    ρ₁ <- maybe (done $ ρ <: freshObj) (translateStatements . (>: (ρ <: freshObj))) mels
                    let Obj {decls = ods', stmts = els} = curr ρ₁
                    let body = P'.Block els
                    -- Construct massive 'if-else' statement out of all if
                    -- cases.
                    ρ₂ <- foldM makeIf (() >: ρ₁ <: (ods', body)) os
                    -- Flip cases, such that the first syntactical Promela case is executed first.
                    let (ods, os') = curr ρ₂
                    let ifStmt = flipIfs $ Pos p os'
                    let obj = curr ρ
                    let obj' = obj {decls = ods ++ decls obj, stmts = ifStmt : stmts obj}
                    translateStatements $ ss >: ρ₂ <: obj'
                  Ok ρ' -> do
                    let (ods', cs, def) = curr ρ'
                    let select' = P'.Select (reverse cs) def
                    let Obj {stmts = oss, decls = ods} = curr ρ
                    let obj = Obj {stmts = Pos p select' : oss, decls = ods ++ ods'}
                    translateStatements $ ss >: ρ' <: obj
          -- Translation of declarations
          P.Decl x t me ->
            let -- Translation of primitive declaration with default zero value.
                primitiveDecl zero = do
                  -- Construct translated variable name.
                  let x' = prefix ρ ++ x
                  -- Translate right-hand side expression, or fall back on zero value.
                  rhs <- maybe (return zero) (translateExp (varenv ρ)) me
                  let Obj {decls = ods, stmts = oss} = curr ρ
                  -- Add declaration to the list of declarations.
                  let obj = Obj {decls = ods ++ [Pos p (P'.Decl x rhs)], stmts = oss}
                  -- Insert the declared name in the variable environment,
                  -- bound to its translated name.
                  let ρ'' = ρ {varenv = M.insert x x' $ varenv ρ}
                  translateStatements (ss >: ρ'' <: obj)
             in case t of
                  -- For channel declarations
                  P.TChan ->
                    -- First ensure that the channel is not a Gomela synthetic channel
                    if not $ L.isPrefixOf "child" x
                      then -- Extract capacity expression
                      case me of
                        Just (P.Chan e) -> do
                          -- Translate capacity expression
                          e' <- translateExp (varenv ρ) e
                          -- Construct translated channel declaration
                          let chdecl = Pos p $ P'.Chan x e'
                          -- Add channel declaration to context declarations
                          let obj' = (curr ρ) {decls = decls (curr ρ) ++ [chdecl]}
                          -- Insert channel in the capacity and variable environments,
                          -- with capacity expression and its own name.
                          let ρ' = ρ {κ = M.insert x e' (κ ρ), chenv = M.insert x x (chenv ρ)}
                          -- Translate the remaining statements
                          translateStatements $ ss >: ρ' <: obj'
                        _ -> err $ "Channel " ++ x ++ " has no capacity."
                      else translateStatements $ ss >: ρ
                  -- For integers, translate primitive declaration with default value 0
                  P.TInt -> primitiveDecl $ P'.CNum 0
                  -- For booleans, translate primitive declaration with default value 'false'
                  P.TBool -> primitiveDecl P'.CFalse
                  -- FIXME: Ignore named types
                  P.TNamed _ -> translateStatements $ ss >: ρ
          P.ExpS (P.Run f es) -> do
            -- Extract callee from call graph
            (ps, ss') <- case Mb.fromJust $ M.lookup f (cg ρ) of
              P.Proc _ ps ss' -> return (ps, ss')
              _ -> err $ "[INVALID FUNCTION] " ++ f ++ " not in call-graph."
            -- Uniquely denominate the calling context by suffixing the call index.
            let f' = f ++ "'" ++ show (calls ρ)
            let -- Associate formal and actual parameters
                pes = zip ps es
                -- Add an initialization statement for each formal-actual parameter pair
                addVarInit ((x, t), e) =
                  let -- Uniquely identify local variable by prefixing the unique denomination.
                      x' = f' ++ "_" ++ x
                      addExp = translateExp (varenv ρ) >=> return . (: []) . Pos p . P'.Decl x'
                   in case t of
                        P.TChan -> return []
                        P.TNamed t' -> case t' of
                          "Chandef" -> return []
                          "Wgdef" -> err "Waitgroups not (yet) supported"
                          "Mutexdef" -> err "Mutexes not (yet) supported"
                          _ -> err $ "Unexpected named type: " ++ t'
                        P.TBool -> addExp e
                        P.TInt -> addExp e
                -- Insert each (treated) primitive formal parameter name in
                -- the local variable environment.
                addVarName ve (x, t) =
                  case t of
                    P.TInt -> M.insert x (f' ++ "_" ++ x) ve
                    P.TBool -> M.insert x (f' ++ "_" ++ x) ve
                    _ -> ve
                -- Bind the formal parameter channel name to the
                -- actual parameter name.
                addCh ce ((a, t), e) =
                  case (t, e) of
                    (P.TChan, P.EVar (P.Var c)) -> M.insert a c ce
                    _ -> ce
            -- Construct all formal parameter declarations.
            initializers <- foldMonad addVarInit [] (++) pes
            -- Context-sensitively translate body of the callee
            let ρ1 =
                  Ctxt
                    { -- Body statements
                      syntax = ss',
                      -- Call graph is identical
                      cg = cg ρ,
                      -- Name prefix of callee-local variables
                      prefix = f' ++ "_",
                      -- Number of calls has increased by 1
                      calls = calls ρ + 1,
                      -- Add all parameter names to the local context.
                      varenv = Prelude.foldl addVarName (varenv ρ) ps,
                      -- Construct a fresh channel environment based on the parameters.
                      chenv = Prelude.foldl addCh M.empty pes,
                      κ = κ ρ,
                      curr = Obj {decls = initializers, stmts = []}
                    }
            ρ2 <- translateStatements ρ1
            let Obj {stmts = oss'} = curr ρ2
            let (ods', s') =
                  if isSequential ss
                    then (decls $ curr ρ2, P'.Block oss')
                    else ([], P'.Go $ ods' ++ oss')
            let Obj {decls = ods, stmts = oss} = curr ρ
            let obj' = Obj {decls = ods ++ ods', stmts = Pos p s' : oss}
            -- Absorb any chanel declarations and calls from the context
            -- produced by translating the callee.
            let ρ3 = ρ {κ = κ ρ2, calls = calls ρ2}
            -- Discard subsequent 'run receiver(c)' statements before
            -- continuing translation.
            let ss'' = skipReceiverRun ss
            translateStatements (ss'' >: ρ3 <: obj')
          -- Non-run expression are considered irrelevant (Only checks for the absence of calls)
          P.ExpS e -> do
            _ <- translateExp (varenv ρ) e
            translateStatements (ss >: ρ)
          P.For r ss' -> do
            -- Obtain translated range expression components:
            -- 1. Loop variable
            -- 2. Translated lower bound
            -- 3. Translated upper bound
            (x, e1', e2') <- translateRange p (varenv ρ) r
            ρ₁ <- translateStatements (ss' >: ρ <: freshObj)
            let Obj {decls = ods, stmts = oss} = curr ρ
            let Obj {decls = ods', stmts = oss'} = curr ρ₁
            -- Add 'for' loop to the list of translated statements
            let oss2 = Pos p (P'.For x e1' e2' P'.Inc oss') : oss
            -- Construct translation object and proceed with the
            -- rest of the translation.
            let obj' = Obj {decls = ods ++ ods', stmts = oss2}
            let ρ₂ = ρ {calls = calls ρ₁, chenv = chenv ρ₁, κ = κ ρ₁}
            translateStatements $ ss >: ρ₂ <: obj'
          -- 'do' statements are not supported
          P.Do _ _ -> err "Unexpected 'do' statement with non-deterministic branches."

-- Translate range statement in for loops
translateRange :: Int -> M.Map String String -> P.Range -> Err (String, P'.Exp, P'.Exp)
translateRange p venv = \case
  -- Only numeric bounds are considered viable
  P.Between x e1 e2 -> do
    -- Translate bound expressions (in order: lower and upper)
    (e1', e2') <- binaryCons (translateExpPos p venv) (,) e1 e2
    -- Produce the loop variable and the bound expressions
    return (x, e1', e2')
  _ -> Bad "Unexpected range over array."

-- Translate expression, taking variable environment into consideration.
-- Uses positional information for debugging and error reporting purposes.
translateExpPos :: Int -> M.Map String String -> P.Exp -> Err P'.Exp
translateExpPos p σ =
  let err = posErr p
      translateExp = translateExpPos p
      bin = binaryCons (translateExp σ)
      un = unaryCons (translateExp σ)
   in \case
        -- Constant expression translation
        P.Const (P.VInt n) -> return $ P'.CNum n
        P.Const (P.VBool False) -> return P'.CFalse
        P.Const (P.VBool True) -> return P'.CTrue
        -- Boolean arithmetic translation
        P.Not e -> un P'.Not e
        P.And e1 e2 -> bin P'.And e1 e2
        P.Or e1 e2 -> bin P'.Or e1 e2
        -- Numeric comparison translation
        P.Le e1 e2 -> bin P'.Le e1 e2
        P.Ge e1 e2 -> bin P'.Ge e1 e2
        P.Lt e1 e2 -> bin P'.Lt e1 e2
        P.Gt e1 e2 -> bin P'.Gt e1 e2
        P.Eq e1 e2 -> bin P'.Eq e1 e2
        P.Ne e1 e2 -> bin P'.Ne e1 e2
        -- Numeric arithmetic translation
        P.Plus e1 e2 -> bin P'.Plus e1 e2
        P.Minus e1 e2 -> bin P'.Minus e1 e2
        P.Mult e1 e2 -> bin P'.Mult e1 e2
        P.Div e1 e2 -> bin P'.Div e1 e2
        P.Neg e -> un P'.Neg e
        -- Variable translation
        P.EVar (P.Var x) -> do
          let errMsg = err $ "[INVALID VARIABLE] binding not found for: " ++ x
          -- Look up appropriate Go variable name for the
          -- given Promela variable in the environment
          maybe errMsg (return . P'.Var) $ M.lookup x σ
        e -> err $ "Promela to Go: Unexpected expression translation: " ++ show e

-- Translate Promela channel operation to Go channel operation
translateOp :: Ctxt (Pos P.Stmt) a -> Err (Ctxt () (Pos P'.Stmt))
translateOp ρ =
  let translate p cons c =
        -- Skip by convention channels preceded by "child".
        -- They are only introduced to model single-threaded function
        -- calls in Promela.
        if "child" `L.isPrefixOf` c
          then done $ ρ <: Pos p P'.Skip
          else -- Look up the Go name for the Promela channel name

            let errMsg = Bad $ "[INVALID CHANNEL] binding not found for: " ++ c
                -- Translate to equivalent Go operation.
                makeCtx = done . (ρ <:) . Pos p . P'.Atomic . cons
                -- Look up channel name in translation context
                c' = M.lookup c (chenv ρ)
             in Mb.maybe errMsg makeCtx c'
   in case syntax ρ of
        -- Translate send statement
        Pos p (P.Send (P.Var c) _) -> translate p P'.Send c
        -- Translate receive statement
        Pos p (P.Recv (P.Var c) _) -> translate p P'.Recv c
        Pos p s -> Bad (":" ++ show p ++ ": Promela-to-Go Translation: Unexpected statement: " ++ show s)

-- Partial translation from Promela constants to Go constant expressions
translateVal :: P.Val -> Maybe P'.Exp
translateVal v = case v of
  P.VInt n -> return $ P'.CNum n
  P.VBool True -> return P'.CTrue
  P.VBool False -> return P'.CFalse
  _ -> Nothing

-- Skips over a statement of the form:
--  run receiver(c)
-- as it is an artificial construct introduced by Gomela.
skipReceiverRun :: [Pos P.Stmt] -> [Pos P.Stmt]
skipReceiverRun = \case
  [] -> []
  Pos _ (P.ExpS (P.Run "receiver" [_])) : ss -> ss
  ss -> ss

-- Checks whether a method invocation is single-threaded or not,
-- by inspecting whether the continuation of a method invocation
-- is followed by a channel receive where the name is prefixed with "child":
--   child_<function>?0
isSequential :: [Pos P.Stmt] -> Bool
isSequential = \case
  (Pos _ (P.Recv (P.Var c) [P.Const (P.VInt 0)])) : _ -> "child" `L.isPrefixOf` c
  _ -> False
