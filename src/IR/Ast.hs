module IR.Ast where

import Utilities.PrettyPrint (PrettyPrint (prettyPrint), indent)

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
  | -- if b then S1 else S2
    If Exp Stmt Stmt
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
  = -- e1 && e2
    And Exp Exp
  | -- e1 || e2
    Or Exp Exp
  | -- not e
    Not Exp
  | -- e1 == e2
    Eq Exp Exp
  | -- e1 != e2
    Ne Exp Exp
  | -- e1 >= e2
    Geq Exp Exp
  | -- e1 > e2
    Gt Exp Exp
  | -- e1 <= e2
    Leq Exp Exp
  | -- e1 < e2
    Lt Exp Exp
  | -- e1 + e2
    Plus Exp Exp
  | -- e1 - e2
    Minus Exp Exp
  | -- e1 * e2
    Mult Exp Exp
  | -- e1 / e2
    Div Exp Exp
  | -- n ∈ ℤ
    Const Int
  | -- true
    BTrue
  | -- false
    BFalse
  | -- x
    Var String
  deriving (Eq, Ord, Read)

instance Show Prog where
  show (Prog cs ps) =
    let showp s = unlines ["go {", prettyPrint 1 s, "}"]
        cs' = unlines (map show cs)
        ps' = unlines (map showp ps)
     in concat [cs', "\n", ps']

instance Show Chan where
  show (Chan c e) = unwords [c, "=", "[" ++ show e ++ "];"]

instance Show Stmt where
  show = prettyPrint 0

instance PrettyPrint Stmt where
  prettyPrint n = \case
    Seq s1 s2 -> unlines [prettyPrint n s1 ++ ";", prettyPrint n s2]
    Skip -> indent n ++ "skip"
    Atomic o -> indent n ++ show o
    If e s1 s2 ->
      unlines
        [ unwords [indent n ++ "if", show e, "{"],
          prettyPrint (n + 1) s1,
          unwords [indent n ++ "} else {"],
          prettyPrint (n + 1) s2,
          indent n ++ "}"
        ]
    For x e1 e2 os ->
      unlines
        [ unwords [indent n ++ "for", x, ":", show e1, "..", show e2, "{"],
          unlines $ map ((indent (n + 1) ++) . (++ ";") . show) os,
          indent n ++ "}"
        ]

instance Show Exp where
  show =
    let bin e1 op e2 = unwords ["(" ++ show e1 ++ ")", op, "(" ++ show e2 ++ ")"]
        un op e = unwords [op, "(" ++ show e ++ ")"]
     in \case
          And e1 e2 -> bin e1 "&" e2
          Or e1 e2 -> bin e1 "|" e2
          Not e -> un "!" e
          Eq e1 e2 -> bin e1 "==" e2
          Ne e1 e2 -> bin e1 "!=" e2
          Geq e1 e2 -> bin e1 ">=" e2
          Gt e1 e2 -> bin e1 ">" e2
          Leq e1 e2 -> bin e1 "<=" e2
          Lt e1 e2 -> bin e1 "<" e2
          Plus e1 e2 -> bin e1 "+" e2
          Minus e1 e2 -> bin e1 "-" e2
          Mult e1 e2 -> bin e1 "*" e2
          Div e1 e2 -> bin e1 "/" e2
          Const n -> show n
          BTrue -> "true"
          BFalse -> "false"
          Var x -> x

instance Show Op where
  show = \case
    Send c -> c ++ "!"
    Recv c -> c ++ "?"

instance ProgramPointOffset Prog where
  ppOffset (Prog _ ss) = sum $ map ppOffset ss

-- Computes the offset required, in terms of program points, to reach
-- the instruction following the channel operation, based on its
-- direction.
--
-- The offsets are:
-- 1. skip: 0 (skip statements are ignored)
-- 2. S1; S2: |S1| + |S2|
-- 3. for x : e1 .. e2 { s }: 2 + |s|
--      1 for the guard
--      1 for the index incrementing operation
-- 4. if e S1 S2 -> 2 + |S1| + |S2|
--      1 for the guard
--      1 for the continuation of the 'then' path
instance ProgramPointOffset Stmt where
  ppOffset = \case
    Skip -> 0
    Seq s1 s2 -> ppOffset s1 + ppOffset s2
    For _ _ _ os -> 2 + sum (map ppOffset os)
    If _ s1 s2 -> 2 + ppOffset s1 + ppOffset s2
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