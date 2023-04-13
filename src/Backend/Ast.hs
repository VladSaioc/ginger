module Backend.Ast where

data Type =
  -- int
    TInt
  -- nat
  | TNat
  -- bool
  | TBool
  -- T
  | TypeVar String
  -- Type -> Type
  | Arrow Type Type
  -- (Type, ...)
  | Tuple [Type]
  deriving (Eq, Ord, Read)

data Pattern =
  -- _
    Wildcard
  -- Constant
  | PCon Const
  deriving (Eq, Ord, Read)

-- Statement
data Stmt =
  -- {x, ...}* := {e, ...}*
    Assign [(String, Exp)]
  -- { S; ... }
  | Block [Stmt]
  -- [ghost] var {x [: T], ...}* := {e, ...}*
  | VarDef Bool [(String, Maybe Type, Exp)]
  -- if e { S } else { S } 
  | If Exp Stmt Stmt
  -- assert e
  | Assert Exp
  -- match e { {case p => S ...}* }
  | MatchStmt Exp [(Pattern, [Stmt])]
  -- while e {invariant e ...}* {decreases e}* { S }
  | While Exp [Exp] [Exp] Stmt
  -- return {e, ...}*
  | Return [Exp]
  deriving (Eq, Ord, Read)

data Const =
  -- -- true
    True 
  -- -- false
  | False
  -- -- n, n ∈ ℤ
  | CNum Integer
  deriving (Eq, Ord, Read)

-- Expressions
data Exp =
  -- -- *
    Any
  -- -- x
  | EVar String
  -- -- const
  | ECon Const
  -- Propositional quantifiers
  -- exists {x [: T], ...}* :: e
  | Exists [(String, Maybe Type)] Exp
  -- forall {x [: T], ...}* :: e
  | Forall [(String, Maybe Type)] Exp
  -- Propositional logic
  -- -- e1 ==> e2
  | Implies Exp Exp
  -- -- e1 <==> e2
  | Equiv Exp Exp
  -- Boolean arithmetic
  -- -- e1 && e2
  | And Exp Exp
  -- -- e1 || e2
  | Or Exp Exp
  -- -- !e
  | Not Exp
  -- Comparison
  -- -- e1 == e2
  | Eq Exp Exp
  -- -- e1 >= e2
  | Geq Exp Exp
  -- -- e1 > e2
  | Gt Exp Exp
  -- -- e1 <= e2
  | Leq Exp Exp
  -- -- e1 < e2
  | Lt Exp Exp
  -- Arithmetic
  -- -- e1 + e2
  | Add Exp Exp
  -- -- e1 - e2
  | Minus Exp Exp
  -- -- e1 * e2
  | Mult Exp Exp
  -- -- e1 % e2
  | Mod Exp Exp
  -- Compound statements
  -- -- if e1 then e2 else e3
  | IfElse Exp Exp Exp
  -- -- match e1 { {case p => e ...}* }
  | Match Exp [(Pattern, Exp)]
  -- -- f({e, ...}*)
  | Call String [Exp]
  deriving (Eq, Ord, Read)

-- Cons({field : type, ...})
data Cons = Cons String [(String, Type)] deriving (Eq, Ord, Read)

data Decl =
  -- datatype x<{Type, ...}> = {Cons | ...}
    Datatype String [Type] [Cons]
  -- type x = Type
  | TypeDecl String Type

  -- [ghost] function f({x : T, ...}*) : T {requires e}* {ensures e}* { e }
  | FunctionDecl Bool String [(String, Type)] [Exp] [Exp] Type Exp
  -- method f({x : T, ...}*) returns ({x : T, ...}*)  {requires e ...}* {ensures e ...}* {decreases e ...}* S
  | MethodDecl String [(String, Type)] [(String, Type)] [Exp] [Exp] [Exp] Stmt
  -- lemma f({x : T, ...}*) returns ({x : T, ...}*)  {requires e ...}* {ensures e ...}* {decreases e ...}* S
  | LemmaDecl String [(String, Type)] [(String, Type)] [Exp] [Exp] [Exp] Stmt
  deriving (Eq, Ord, Read)

-- Back-end program
newtype Program = Program [Decl] deriving (Eq, Ord, Read)