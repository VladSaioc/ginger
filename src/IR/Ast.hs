module IR.Ast where

-- {c = [e]; ...}* {go { S } ...}*
data Prog = Prog [Chan] [Stmt] deriving (Eq, Ord, Read)

-- c = [e]
data Chan = Chan String Exp deriving (Eq, Ord, Read)

data Stmt =
  -- S1; S2
    Seq Stmt Stmt
  -- for (x : e) { s }
  | For String Exp [Op]
  -- c! | c?
  | Atomic Op
  deriving (Eq, Ord, Read)

data Op =
  -- c!
    Send String
  -- c?
  | Recv String
  deriving (Eq, Ord, Read)

data Exp =
  -- e1 + e2
    Plus Exp Exp
  -- e1 - e2
  | Minus Exp Exp
  -- n ∈ ℤ
  | Const Int
  -- x
  | Var String
  deriving (Eq, Ord, Read)
