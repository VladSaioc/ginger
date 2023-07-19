module IR.Ast where

-- Every syntactical construct that may be converted to program
-- points must implement program point offset.
class ProgramPointOffset a where
  ppOffset :: a -> Int

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
        ( unwords ["for", x, ":", show e1, "..", show e2, "{"] : map (("    " ++) . (++ ";") . show) os
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

instance ProgramPointOffset Prog where
  ppOffset (Prog _ ss) = sum $ map ppOffset ss

instance ProgramPointOffset Stmt where
  ppOffset = \case
    Skip -> 0
    Seq s1 s2 -> ppOffset s1 + ppOffset s2
    For _ _ _ os -> 2 + sum (map ppOffset os)
    Atomic o -> ppOffset o

-- Computes the offset required, in terms of program points, to reach
-- the instruction following the channel operation, based on its
-- direction.
--
-- The offsets are:
-- 1. Send: 2 (send + synchronization rendezvous)
-- 2. Receive: 1 (receive)
instance ProgramPointOffset Op where
  ppOffset = \case
    Send _ -> 2
    Recv _ -> 1