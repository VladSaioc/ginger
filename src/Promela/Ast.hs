module Promela.Ast where

import Data.List (intercalate)
import Utilities.Position
import Utilities.PrettyPrint

type Ident = String

newtype Spec = Spec [Module] deriving (Eq, Ord, Read)

data Module
  = TopDecl Ident Type Val
  | Init [Pos Stmt]
  | Proc Ident [Param] [Pos Stmt]
  | Typedef Ident [Field]
  deriving (Eq, Ord, Read)

type Field = (Ident, Type, Maybe Exp)

type Param = (Ident, Type)

type Case = (Pos Stmt, [Pos Stmt])

data Stmt
  = Decl Ident Type (Maybe Exp)
  | If [(Pos Stmt, [Pos Stmt])] (Maybe [Pos Stmt])
  | Do [(Pos Stmt, [Pos Stmt])] (Maybe [Pos Stmt])
  | For Range [Pos Stmt]
  | As LVal Exp
  | Goto Ident
  | Break
  | Skip
  | Assert Exp
  | Recv LVal [Exp]
  | Send LVal [Exp]
  | ExpS Exp
  | Label Ident
  deriving (Eq, Ord, Read)

data Exp
  = Chan Exp
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
  deriving (Eq, Ord, Read)

data Range
  = Between Ident Exp Exp
  | In Ident Ident
  deriving (Eq, Ord, Read)

data LVal
  = Var Ident
  | Field LVal Ident
  | Arr LVal Exp
  deriving (Eq, Ord, Read)

data Val = Free | VInt Int | VBool Bool deriving (Eq, Ord, Read)

data Type
  = TInt
  | TBool
  | TChan
  | TNamed Ident
  deriving (Eq, Ord, Read)

block :: PrettyPrint a => Int -> [a] -> String
block n ss = unlines $ map ((++ ";") . prettyPrint n) ss

instance Show Spec where
  show (Spec ms) = unlines $ map show ms

instance Show Module where
  show = \case
    TopDecl x _ v -> unwords ["#define ", x, show v]
    Init ss -> unlines $ ["init {"] ++ map (prettyPrint 1) ss ++ ["}"]
    Proc f ps ss ->
      let params = intercalate "; " $ map show ps
          header = unwords ["proctype", f ++ "(" ++ params ++ ")", "{"]
       in unlines $ header : map (prettyPrint 1) ss
    Typedef x fs -> unwords ["typedef", x ++ "{", unlines $ map (prettyPrintField 1) fs, "}"]

instance Show Stmt where
  show = prettyPrint 0

instance PrettyPrint Param where
  prettyPrint _ (x, t) = unwords [show t, x]

instance PrettyPrint Stmt where
  prettyPrint n =
    let makeElse =
          maybe
            ""
            ( \ss ->
                unlines
                  [ indent n ++ "else  ->",
                    block (n + 1) ss
                  ]
            )
     in \case
          Decl x t me -> prettyPrintField n (x, t, me)
          If os els ->
            unlines
              [ indent n ++ "if",
                unlines $ map (prettyPrintCase n) os,
                makeElse els,
                indent n ++ "fi"
              ]
          Do os els ->
            unlines
              [ indent n ++ "do",
                unlines $ map (prettyPrintCase n) os,
                makeElse els,
                indent n ++ "od"
              ]
          For r ss ->
            unlines
              [ indent n ++ "for(" ++ show r ++ ") {",
                unlines $ map (prettyPrint $ n + 1) ss,
                indent n ++ "}"
              ]
          As x e -> unwords [indent n ++ show x, "=", show e]
          Goto l -> unwords ["goto", l]
          Break -> indent n ++ "break"
          Skip -> indent n ++ "skip"
          Assert e -> unwords [indent n ++ "assert", show e]
          Recv c es -> indent n ++ show c ++ "?" ++ intercalate ", " (map show es)
          Send c es -> indent n ++ show c ++ "!" ++ intercalate ", " (map show es)
          ExpS e -> indent n ++ show e
          Label l -> unwords [indent n ++ l ++ ":", "skip"]

instance Show Range where
  show = \case
    Between x e1 e2 -> unwords [x, ":", show e1, "..", show e2]
    In x1 x2 -> unwords [x1, "in", x2]

prettyPrintCase :: (Show a1, PrettyPrint a2) => Int -> (a1, [a2]) -> String
prettyPrintCase n (c, ss) =
  unlines
    [ unwords [indent n ++ "::", show c, "->"],
      block (n + 1) ss
    ]

instance Show Val where
  show = \case
    Free -> "??"
    VInt n -> show n
    VBool True -> "true"
    VBool False -> "false"

instance Show Type where
  show = \case
    TInt -> "int"
    TBool -> "bool"
    TChan -> "chan"
    TNamed x -> x

prettyPrintField :: (Show a1, Show a2) => Int -> (String, a1, Maybe a2) -> String
prettyPrintField n (f, t, me) =
  let e = maybe "" show me
   in unwords [indent n ++ show t, f, e ++ ";"]

instance Show Exp where
  show =
    let bin e1 op e2 = unwords ["(" ++ show e1 ++ ")", op, "(" ++ show e2 ++ ")"]
        un op e = op ++ "(" ++ show e ++ ")"
     in \case
          Chan e -> unwords ["[" ++ show e ++ "]", "of", "{?}"]
          Const v -> show v
          And e1 e2 -> bin e1 "&&" e2
          Or e1 e2 -> bin e1 "||" e2
          Eq e1 e2 -> bin e1 "==" e2
          Ne e1 e2 -> bin e1 "!=" e2
          Le e1 e2 -> bin e1 "<=" e2
          Lt e1 e2 -> bin e1 "<" e2
          Ge e1 e2 -> bin e1 ">=" e2
          Gt e1 e2 -> bin e1 ">" e2
          Plus e1 e2 -> bin e1 "+" e2
          Minus e1 e2 -> bin e1 "-" e2
          Mult e1 e2 -> bin e1 "*" e2
          Div e1 e2 -> bin e1 "/" e2
          Neg e -> un "-" e
          Not e -> un "1" e
          Len v -> un "len" v
          EVar v -> show v
          Run f es -> unwords ["run", f ++ "(" ++ intercalate ", " (map show es) ++ ")"]

instance Show LVal where
  show = \case
    Var x -> x
    Field x f -> show x ++ "." ++ f
    Arr x e -> show x ++ "[" ++ show e ++ "]"