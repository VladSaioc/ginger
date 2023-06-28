module Go.Ast where

import Utilities.PrettyPrint (PrettyPrint, indent, prettyPrint)

newtype Prog = Prog [Stmt] deriving (Eq, Ord, Read)

data Stmt
  = Skip
  | Return
  | Chan String Exp
  | Break
  | Atomic CommOp
  | As String Exp
  | Close String
  | Block [Stmt]
  | If Exp [Stmt] [Stmt]
  | Select [(CommOp, [Stmt])] (Maybe [Stmt])
  | For String Exp Exp Diff [Stmt]
  | While Exp [Stmt]
  | Go [Stmt]
  deriving (Eq, Ord, Read)

data Diff = Inc | Dec deriving (Eq, Ord, Read)

data CommOp
  = Send String
  | Recv String
  | Star
  deriving (Eq, Ord, Read)

data Exp
  = CNum Int
  | True
  | False
  | And Exp Exp
  | Or Exp Exp
  | Not Exp
  | Eq Exp Exp
  | Ne Exp Exp
  | Le Exp Exp
  | Lt Exp Exp
  | Ge Exp Exp
  | Gt Exp Exp
  | Plus Exp Exp
  | Minus Exp Exp
  | Mult Exp Exp
  | Neg Exp
  | Var String
  | Div Exp Exp
  deriving (Eq, Ord, Read)

instance Show Prog where
  show (Prog ss) =
    let showp s = unlines $ map (prettyPrint 0) s
     in showp ss

instance PrettyPrint Stmt where
  prettyPrint n s =
    let block n' = unlines . map (prettyPrint $ n + n')
        tab = (++) $ indent n
     in case s of
          Skip -> tab "skip"
          Break -> tab "break"
          Return -> tab "return"
          As x e -> tab x ++ " = " ++ show e
          Chan c e -> unwords [tab c, ":=", "make(chan,", show e ++ ")"]
          Atomic o -> tab $ show o
          Close c -> tab "close(" ++ c ++ ")"
          Block ss -> block 0 ss
          If e s1 s2 ->
            let s2' = [unwords [tab "}, else", unlines ["{", block 1 s2]] | s2 /= []]
             in unlines $
                  [ tab $ unwords ["if", show e, "{"],
                    block 1 s1
                  ]
                    ++ s2'
                    ++ [tab "}"]
          Go ss -> unlines [tab "go {", block 1 ss, tab "}"]
          While e ss ->
            unlines
              [ unwords [tab "for", show e, "{"],
                block 1 ss,
                tab "}"
              ]
          Select cs dfs ->
            let showCase (c, ss) =
                  unlines
                    [ unwords [indent (n + 1) ++ "case", show c ++ ":"],
                      block 2 ss
                    ]
                defCase = case dfs of
                  Nothing -> []
                  Just dfs' -> [unlines [indent (n + 1) ++ "default:", block 2 dfs']]
             in unlines $
                  [tab "select {"]
                    ++ map showCase cs
                    ++ defCase
                    ++ [tab "}"]
          For x e1 e2 diff ss ->
            unlines
              [ unwords [tab "for", x, ":=", show e1 ++ ";", x, "<=", show e2 ++ ";", x ++ show diff, "{"],
                block 1 ss,
                tab "}"
              ]

instance Show Exp where
  show =
    let bin e1 op e2 = unwords ["(" ++ show e1 ++ ")", op, "(" ++ show e2 ++ ")"]
        un op e = op ++ "(" ++ show e ++ ")"
     in \case
          Go.Ast.True -> "true"
          Go.Ast.False -> "false"
          And e1 e2 -> bin e1 "&&" e2
          Or e1 e2 -> bin e1 "||" e2
          Le e1 e2 -> bin e1 "<=" e2
          Lt e1 e2 -> bin e1 "<" e2
          Ge e1 e2 -> bin e1 ">=" e2
          Gt e1 e2 -> bin e1 ">" e2
          Eq e1 e2 -> bin e1 "==" e2
          Ne e1 e2 -> bin e1 "!=" e2
          Not e -> un "!" e
          Neg e -> un "-" e
          Plus e1 e2 -> bin e1 "+" e2
          Minus e1 e2 -> bin e1 "-" e2
          Mult e1 e2 -> bin e1 "*" e2
          Div e1 e2 -> bin e1 "/" e2
          CNum n -> show n
          Var x -> x

instance Show CommOp where
  show = \case
    Send c -> c ++ "<-"
    Recv c -> "<-" ++ c
    Star -> "*"

instance Show Diff where
  show = \case
    Inc -> "++"
    Dec -> "--"