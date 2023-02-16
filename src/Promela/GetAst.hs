module Promela.GetAst (getAst) where

import Promela.ParPromela
import qualified Promela.AbsPromela as Raw

import Promela.Ast
import Promela.ErrM as PromelaResult

import Utilities.Err as U
import Utilities.General
import Utilities.Position

(#) :: Raw.NUMBER -> Integer
(#) (Raw.NUMBER (_, i)) = read i::Integer

(&) :: Raw.ID -> Ident
(&) (Raw.ID (_, x)) = x

(~) :: Raw.ID -> Int
(~) (Raw.ID ((l, _), _)) = l

notImplemented :: Show a1 => a1 -> U.Err a2
notImplemented err = U.Bad ("Not implemented: " ++ show err)

(@@) :: ((Int, Int), String) -> a -> Pos a
((l, _), _) @@ a = Pos l a

pToken :: ((Int, Int), String) -> Pos ()
pToken ((l, _), _) = Pos l ()

pIdent :: Monad m => Raw.ID -> m Ident
pIdent x = return ((&) x)

-- Parses the given string as a Promela program and
-- performs additional refinement on the existing parse tree.
getAst :: String -> U.Err Spec
getAst = pProgram . pSpec . myLexer

-- Refines a BNFC-produced Promela AST into a more ergonomic representation.
-- It performs minimal semantic checks during refinement. 
pProgram :: PromelaResult.Err Raw.Spec -> U.Err Spec 
pProgram = \case
  PromelaResult.Ok (Raw.Program modules) -> do
    ms <- results (map pModule modules)
    return (Spec ms)
  PromelaResult.Bad err -> U.Bad err

-- Refines the ASTs of top-level declarations (e. g. procs, types)
-- into a more ergonomic representation. 
pModule :: Raw.Module -> U.Err Module
pModule = \case
-- #define x e, where e ∈ ℤ
  Raw.Define _ x e -> do
    x <- pIdent x
    v <- pVal e
    return (TopDecl x TInt v)
-- init { ss } 
  Raw.Init _ ss -> do
    ss <- results (map pStep ss)
    return (Init ss)
-- proctype f(ps) { ss }
  Raw.Proctype _ f ps ss -> do
    f <- pIdent f
    ps <- results (map pParam ps)
    ss <- results (map pStep ss)
    return (Proc f ps ss)
-- typedef t { fs } 
  Raw.Typedef _ t fs -> do
    t <- pIdent t
    fs <- results (map pField fs)
    return (Typedef t fs)
  m -> notImplemented m 

-- Refine expressions to constant values. Will fail
-- if the given expression is not a constant.
pVal :: Raw.Exp -> U.Err Val
pVal = \case
-- Extracts a value from e, where e = -n and n is a natural number
  Raw.ExpNeg _ (Raw.ExpConst (Raw.CInt n)) ->
    return (VInt (0 - ((#)n)))
-- Extracts a value from e, where e = n and n is a natural number
  Raw.ExpConst (Raw.CInt i) -> return (VInt ((#)i))
  _ -> U.Bad "Bad #define macro"

-- Refine procedure parameter members of the AST. A parameter
-- is a pair between a name and a type, with positional information.
pParam :: Raw.Param -> U.Err Param
pParam (Raw.Param (Raw.Decl t x d)) = case d of
  Raw.DBodyEmpty -> do
    x <- pIdent x
    return (x, pType t)
  _ -> U.Bad "Bad parameter definition"

-- Refine a typedef field
pField :: Raw.Field -> U.Err Field
pField (Raw.Field d@(Raw.Decl t x b)) = case b of
  Raw.DBodyEmpty -> U.Bad "Bad field declaration"
  _ -> do
    Pos _ d <- pDecl d
    case d of
      Decl x t e -> return (x, t, e)
      _ -> U.Bad "Bad field declaration"

pStep :: Raw.Step -> U.Err (Pos Stmt)
pStep = \case
  Raw.StepDecl d -> pDecl d
  Raw.StepStmt s -> pStmt s

pDecl :: Raw.Decl -> U.Err (Pos Stmt)
pDecl (Raw.Decl t x b) =
  let l = (~) x
  in do
    x <- pIdent x
    (t, e) <- pDeclBody (pType t) b
    return (l @ Decl x t e)

pDeclBody :: Type -> Raw.DeclBody -> U.Err (Type, Maybe Exp)
pDeclBody t b = case (t, b) of
  -- Integer variable declarations
  (TInt, Raw.DBodyEmpty) ->
    let v = return (Const (VInt 0))
    in return (t, v)
  (TInt, Raw.DBodyExp _ re) ->
    let Pos _ e = pExp re
    in return (t, return e)
  -- Boolean variable declarations
  (TBool, Raw.DBodyEmpty) ->
    let v = return (Const (VBool False))
    in return (t, v)
  (TBool, Raw.DBodyExp _ re) ->
    let Pos _ e = pExp re 
    in return (t, return e)
  -- User-defined typed variable declarations
  (TNamed _, Raw.DBodyEmpty) -> return (t, Nothing)
  -- Channel declarations
  (TChan, Raw.DBodyChan _ n _ _) ->
    let ch = return (Chan ((#) n))
    in return (TChan, ch)
  _ -> U.Bad "Bad variable declaration"

pStmt :: Raw.Stmt -> U.Err (Pos Stmt)
pStmt =
  let
    desugarOff c v = do
      v <- pLVal v
      offset <- return (c (EVar v) (Const (VInt 1)))
      return (As v offset)
  in \case
    -- if :: branches... fi
    Raw.StmtIf (Raw.IF l) os -> do
      els <- getElse os
      os <- pOptions os
      return (l @@ If os els)
    -- do :: branches... od
    Raw.StmtDo (Raw.DO l) os -> do
      els <- getElse os
      os <- pOptions os
      return (l @@ Do os els)
    -- for (range) { ... }
    Raw.StmtFor (Raw.FOR l) r ss -> do
      Pos _ r <- pRange r
      ss <- results (map pStep ss)
      return (l @@ For r ss)
    -- goto label
    Raw.StmtGoto (Raw.GOTO l) label -> return (l @@ Goto ((&) label))
    -- label: s
    Raw.StmtLabel label _ s -> do
      l <- return ((~) label)
      label <- return ((&) label)
      s <- pStmt s
      return (l @ Label label s)
    -- v = e
    Raw.StmtAssign v _ e -> return (do
      v <- pLVal v
      e <- pExp e
      return (As v e))
    -- x++ : Desugared into x = x + 1
    Raw.StmtIncr v _ -> return (desugarOff Plus v)
    -- x-- : Desugared into x = x - 1
    Raw.StmtDecr v _ -> return (desugarOff Minus v)
    -- skip
    Raw.StmtSkip (Raw.SKIP l) -> return (l @@ Skip)
    -- break
    Raw.StmtBreak (Raw.BREAK l) -> return (l @@ Break)
    -- assert e
    Raw.StmtAssert _ e -> return (unaryCons pCond Assert e)
    -- c?e
    Raw.StmtRcv v _ es -> return (do
      v <- pLVal v
      es <- results (map pExp es)
      return (Rcv v es))
    -- c!e
    Raw.StmtSend v _ es -> return (do
      v <- pLVal v
      unaryCons results (Rcv v) (map pExp es))
    -- e
    Raw.StmtExpr c -> return (unaryCons pCond ExpS c)

getElse :: [Raw.Option] -> U.Err (Maybe [Pos Stmt])
getElse =
  let f = \case
          Raw.OptionSt _ -> id
          Raw.OptionEls _ ss -> \e -> do
            e <- e
            els <- results (map pStep ss)
            case e of
              Just _ -> U.Bad "Branch list has more than one else."
              Nothing -> return (Just els)
  in foldr f (return Nothing)

pOptions :: [Raw.Option] -> U.Err [[Pos Stmt]]
pOptions os =
  let f = \case
          Raw.OptionSt ss -> \steps -> (results (map pStep ss)) : steps
          _ -> id
  in results (foldr f [] os)

pRange :: Raw.Range -> U.Err (Pos Range)
pRange = \case
  Raw.RangeExps x (Raw.COL l) e1 _ e2 -> return (do
    _ <- pToken l
    x <- pIdent x
    binaryCons pExp (Between x) e1 e2)
  Raw.RangeArrs x1 (Raw.IN l) x2 -> return (do
    _ <- pToken l
    binaryCons pIdent In x1 x2)

pType :: Raw.Type -> Type
pType = \case
  Raw.TypeChan _ -> TChan
  Raw.TypeInt _ -> TInt
  Raw.TypeBool _ -> TBool
  Raw.TypeNamed (Raw.ID (_, x)) ->  TNamed x

pCond :: Raw.Cond -> Pos Exp
pCond =
  let bin = binaryCons pCond
  in \case
    Raw.CondOr e1 _ e2 -> bin Or e1 e2
    Raw.CondAnd e1 _ e2 -> bin And e1 e2
    Raw.CondExp e -> pExp e 

pExp :: Raw.Exp -> Pos Exp
pExp =
  let bin = binaryCons pExp
  in \case
    -- e1 == e2
    Raw.ExpEq e1 _ e2 -> bin Eq e1 e2
    -- e1 != e2
    Raw.ExpNe e1 _ e2 -> bin Ne e1 e2
    -- e1 <= e2
    Raw.ExpLe e1 _ e2 -> bin Le e1 e2
    -- e1 >= e2
    Raw.ExpGe e1 _ e2 -> bin Ge e1 e2
    -- e1 < e2
    Raw.ExpLt e1 _ e2 -> bin Lt e1 e2
    -- e1 > e2
    Raw.ExpGt e1 _ e2 -> bin Gt e1 e2
    -- e1 + e2
    Raw.ExpPlus e1 _ e2 -> bin Plus e1 e2
    -- e1 - e2
    Raw.ExpMinus e1 _ e2 -> bin Minus e1 e2
    -- e1 * e2
    Raw.ExpProd e1 _ e2 -> bin Mult e1 e2
    -- e1 / e2
    Raw.ExpDiv e1 _ e2 -> bin Div e1 e2
    -- -e1
    Raw.ExpNeg _ e -> unaryCons pExp Neg e
    -- !e1
    Raw.ExpNot _ e -> unaryCons pExp Not e
    -- len(v)
    Raw.ExpLen _ v -> unaryCons pLVal Len v
    -- v
    Raw.ExpLVal v -> unaryCons pLVal EVar v
    -- c
    Raw.ExpConst c -> unaryCons pConst Const c
    -- run f (es...)
    Raw.ExpRun (Raw.RUN tok) f es -> do
      _ <- pToken tok
      f <- pIdent f
      es <- results (map pExp es)
      return (Run f es)

pLVal :: Raw.LVal -> Pos LVal
pLVal = \case
  Raw.ValId x -> ((~) x) @ Var ((&) x)
  Raw.ValArr x e -> do
    x <- pLVal x
    unaryCons pExp (Arr x) e
  Raw.ValField x _ f -> do
    x <- pLVal x
    return (Field x ((&) f))

pConst :: Raw.Const -> Pos Val
pConst = \case
  Raw.CInt n@(Raw.NUMBER ((l, _), _)) ->
    l @ (VInt ((#) n))
  Raw.CTrue (Raw.TRUE ((l, _), _)) -> l @ (VBool True)
  Raw.CFalse (Raw.FALSE ((l, _), _)) -> l @ (VBool False)