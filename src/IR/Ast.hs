module IR.Ast where

-- {c = [e]; ...}* {go { S } ...}*
data Prog = Prog [Chan] [Stmt] deriving (Eq, Ord, Read)

-- c = [e]
data Chan = Chan String Exp deriving (Eq, Ord, Read)

data Stmt
  = -- S1; S2
    Seq Stmt Stmt
  | -- skip
    Skip
  | -- for (x : e .. e) { s }
    For String Exp Exp [Op]
  | -- c! | c?
    Atomic Op
  deriving (Eq, Ord, Read)

data Op
  = -- c!
    Send String
  | -- c?
    Recv String
  deriving (Eq, Ord, Read)

data Exp
  = -- e1 + e2
    Plus Exp Exp
  | -- e1 - e2
    Minus Exp Exp
  | -- e1 * e2
    Mult Exp Exp
  | -- e1 / e2
    Div Exp Exp
  | -- n ∈ ℤ
    Const Int
  | -- x
    Var String
  deriving (Eq, Ord, Read)

instance Show Prog where
  show (Prog cs ps) =
    let showp s = unlines ["go {", show s, "}"]
        cs' = unlines (map show cs)
        ps' = unlines (map showp ps)
     in concat [cs', "\n", ps']

instance Show Chan where
  show (Chan c e) = unwords [c, "=", "[" ++ show e ++ "];"]

instance Show Stmt where
  show = \case
    Seq s1 s2 -> unlines ["  " ++ show s1 ++ ";", "  " ++ show s2]
    Skip -> "skip"
    Atomic o -> "  " ++ show o
    For x e1 e2 os ->
      unlines
        ( [unwords ["for", x, ":", show e1, "..", show e2, "{"]]
            ++ map (\o -> ("    " ++ show o ++ ";")) os
        )
        ++ "  }"

instance Show Exp where
  show =
    let bin e1 op e2 = unwords ["(" ++ show e1 ++ ")", op, "(" ++ show e2 ++ ")"]
     in \case
          Plus e1 e2 -> bin e1 "+" e2
          Minus e1 e2 -> bin e1 "-" e2
          Mult e1 e2 -> bin e1 "*" e2
          Div e1 e2 -> bin e1 "/" e2
          Const n -> show n
          Var x -> x

instance Show Op where
  show = \case
    Send c -> c ++ "!"
    Recv c -> c ++ "?"