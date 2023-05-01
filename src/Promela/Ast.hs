module Promela.Ast where

import Utilities.Position

type Ident = String

newtype Spec = Spec [Module] deriving (Eq, Ord, Read, Show)

data Module
  = TopDecl Ident Type Val
  | Init [Pos Stmt]
  | Proc Ident [Param] [Pos Stmt]
  | Typedef Ident [Field]
  deriving (Eq, Ord, Read, Show)

type Field = (Ident, Type, Maybe Exp)

type Param = (Ident, Type)

data Stmt
  = Decl Ident Type (Maybe Exp)
  | If [[Pos Stmt]] (Maybe [Pos Stmt])
  | Do [[Pos Stmt]] (Maybe [Pos Stmt])
  | For Range [Pos Stmt]
  | As LVal Exp
  | Goto Ident
  | Break
  | Skip
  | Assert Exp
  | Rcv LVal [Exp]
  | Send LVal [Exp]
  | ExpS Exp
  | Label Ident (Pos Stmt)
  deriving (Eq, Ord, Read, Show)

data Exp
  = Chan Int
  | Const Val
  | And Exp Exp
  | Or Exp Exp
  | Eq Exp Exp
  | Ne Exp Exp
  | Le Exp Exp
  | Lt Exp Exp
  | Ge Exp Exp
  | Gt Exp Exp
  | Plus Exp Exp
  | Minus Exp Exp
  | Mult Exp Exp
  | Div Exp Exp
  | Neg Exp
  | Not Exp
  | Len LVal
  | EVar LVal
  | Run Ident [Exp]
  deriving (Eq, Ord, Read, Show)

data Range
  = Between Ident Exp Exp
  | In Ident Ident
  deriving (Eq, Ord, Read, Show)

data LVal
  = Var Ident
  | Field LVal Ident
  | Arr LVal Exp
  deriving (Eq, Ord, Read, Show)

data Val = Free | VInt Int | VBool Bool deriving (Eq, Ord, Read, Show)

data Type
  = TInt
  | TBool
  | TChan
  | TNamed Ident
  deriving (Eq, Ord, Read, Show)