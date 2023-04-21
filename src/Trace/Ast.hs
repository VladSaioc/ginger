module Trace.Ast where

import Data.Map.Strict
import Data.Set
import Utilities.Position

type Ident = String

type Pid = Int

data Trace m = Trace
  { steps :: [m Step],
    notTerminated :: Set Int,
    processes :: Map Int (Ident, [m Step])
  }

data Step = Step Pid Stmt deriving (Eq, Ord, Show, Read)

data Stmt
  = Start Pid Ident [Exp]
  | Goto String
  | As LVal Exp
  | Send LVal [Exp]
  | Rcv LVal [Exp]
  | Expr Exp
  | Assert Exp
  deriving (Eq, Ord, Show, Read)

data Exp
  = Or Exp Exp
  | And Exp Exp
  | Eq Exp Exp
  | Ne Exp Exp
  | Le Exp Exp
  | Ge Exp Exp
  | Lt Exp Exp
  | Gt Exp Exp
  | Plus Exp Exp
  | Minus Exp Exp
  | Prod Exp Exp
  | Div Exp Exp
  | Neg Exp
  | Not Exp
  | Len LVal
  | Var LVal
  | Const Int
  deriving (Eq, Ord, Show, Read)

data LVal = Id Ident | Arr LVal Exp | Field LVal Ident
  deriving (Eq, Ord, Show, Read)