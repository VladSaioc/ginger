module Backend.Ast where

import Data.List (intercalate)
import Utilities.PrettyPrint

-- | Back-end type syntax:
--
-- > T ::= int | nat | bool | x | T -> T | ({T, ...}*)
data Type
  = TBad
  | -- int
    TInt
  | -- nat
    TNat
  | -- bool
    TBool
  | -- T
    TVar String
  | -- Type -> Type
    Type :-> Type
  | -- (Type, ...)
    Tuple [Type]
  deriving (Eq, Ord, Read)

-- | Back-end pattern matching syntax:
--
-- > P ::= _ | c | x | C(p, ...) | (p, ...)
data Pattern
  = -- | >  _
    Wildcard
  | -- | > c
    PCon Const
  | -- | > x
    PVar String
  | -- | > C(p, ...)
    PAdt String [Pattern]
  | -- | > (p, ...)
    PTuple [Pattern]
  deriving (Eq, Ord, Show, Read)

-- | Back-end statement syntax:
--
-- > S ::= {x, ...}* := {e, ...}*
-- >    | { S; ... }
-- >    | [ghost] var {x [: T], ...}* := {e, ...}*
-- >    | if e { S } else { S }
-- >    | assert e
-- >    | match e { {case p => S ...}* }
-- >    | while e {invariant e ...}* {decreases e}* { S }
-- >    | return {e, ...}*
data Stmt
  = -- | > {x, ...}* := {e, ...}*
    Assign [(String, Exp)]
  | -- | > { S; ... }
    Block [Stmt]
  | -- | > [ghost] var {x [: T], ...}* := {e, ...}*
    VarDef Bool [(String, Maybe Type, Exp)]
  | -- | > if e { S } else { S }
    If Exp Stmt (Maybe Stmt)
  | -- | > assert e
    Assert Exp
  | -- | > match e { {case p => S ...}* }
    MatchStmt Exp [(Pattern, Stmt)]
  | -- | > while e {invariant e ...}* {decreases e}* { S }
    While Exp [Exp] [Exp] Stmt
  | -- | > return {e, ...}*
    Return [Exp]
  deriving (Eq, Ord, Show, Read)

-- | Back-end constants:
-- > c ::= true | false | n
data Const
  = -- | > true
    CTrue
  | -- | > false
    CFalse
  | -- | > n ∈ ℤ
    CNum Int
  deriving (Eq, Ord, Show, Read)

-- | Back-end expressions:
--
-- > e ::= match e1 { {case p => e ...}* }
-- >    | if e1 then e2 else e3
-- >    | exists {x [: T], ...}* :: e
-- >    | forall {x [: T], ...}* :: e
-- >    | e1 in e2
-- >    | {{e, ...} *}
-- >    | e1 <==> e2 | e1 ==> e2
-- >    | e1 && e2 | e1 || e2
-- >    | e1 == e2 | e1 != e2
-- >    | e1 >= e2 | e1 > e2
-- >    | e1 <= e2 | e1 < e2
-- >    | exists {x [: T], ...}* :: e
data Exp
  = -- COMPOUND EXPRESSIONS
    -- | > match e1 { {case p => e ...}* }
    Match Exp [(Pattern, Exp)]
  | -- | > if e1 then e2 else e3
    IfElse Exp Exp Exp
  | -- PROPOSITIONAL QUANTIFIERS
    -- | > exists {x [: T], ...}* :: e
    Exists [(String, Maybe Type)] Exp
  | -- | > forall {x [: T], ...}* :: e
    Forall [(String, Maybe Type)] Exp
  | -- | > e1 in e2
    In Exp Exp
  | -- | > {{e, ...} *}
    ESet [Exp]
  | -- BINARY OPERATORS
    -- Propositional logic
    -- | > e1 <==> e2
    Exp :<==> Exp
  | -- | > e1 ==> e2
    Exp :==> Exp
  | -- Boolean arithmetic
    -- | > e1 && e2
    Exp :&& Exp
  | -- | > e1 || e2
    Exp :|| Exp
  | -- Comparison
    -- | > e1 == e2
    Exp :== Exp
  | -- | > e1 != e2
    Exp :!= Exp
  | -- | > e1 >= e2
    Exp :>= Exp
  | -- | > e1 > e2
    Exp :> Exp
  | -- | > e1 <= e2
    Exp :<= Exp
  | -- | > e1 < e2
    Exp :< Exp
  | -- Arithmetic
    -- | > e1 + e2
    Exp :+ Exp
  | -- | > e1 - e2
    Exp :- Exp
  | -- | > e1 * e2
    Exp :* Exp
  | -- | > e1 / e2
    Exp :/ Exp
  | -- | > e1 % e2
    Exp :% Exp
  | -- UNARY OPERATORS
    -- | > !e
    Not Exp
  | -- | > (e1, ... en)
    ETuple [Exp]
  | -- TERMINAL EXPRESSIONS
    -- | > *
    Any
  | -- | > x
    EVar String
  | -- | > c
    ECon Const
  | -- | > f({e, ...}*)
    Call String [Exp]
  deriving (Eq, Ord, Show, Read)

-- | Back-end record type definition:
--
-- > T({field : type, ...}*)
data Cons = Cons String [(String, Type)] deriving (Eq, Ord, Read)

-- | Back-end Hoare triple syntax. Uses holes for keyword and return type:
--
-- > H<_, _> ::= _ x[\<{T, ...}>]({x : T, ...}*) _
-- >      {requires e\n...}*
-- >      {ensures e\n...}*
-- >      {decreases e\n...}*
data HoareWrap = HoareWrap
  { ghost :: Bool,
    name :: String,
    types :: [Type],
    params :: [(String, Type)],
    decreases :: [Exp],
    requires :: [Exp],
    ensures :: [Exp]
  }
  deriving (Eq, Ord, Read)

-- | Back-end function declaration syntax:
--
-- > F ::= H<[ghost] function, : T> E
data Function = Function
  { yields :: Type,
    funcHoare :: HoareWrap,
    funcBody :: Exp
  }
  deriving (Eq, Ord, Read)

-- | Method declaration syntax:
--
-- > M ::= H<lemma | method, returns ({ x : T, ...}*)> { {S; ...}* }
data Method = Method
  { returns :: [(String, Type)],
    methodHoare :: HoareWrap,
    methodBody :: Stmt
  }
  deriving (Eq, Ord, Read)

-- | Back-end top-level declaration:
--
-- > D ::= datatype x<{T, ...}> = {Cons | ...}
-- >    | type x = T
data Decl
  = -- | > datatype x<{Type, ...}> = {Cons | ...}
    Datatype String [Type] [Cons]
  | -- | > type x = Type
    TypeDecl String Type
  | -- [ghost] function f({x : T, ...}*) : T {requires e}* {ensures e}* { e }
    FDecl Function
  | -- (lemma | method) f({x : T, ...}*) returns ({x : T, ...}*)  {requires e ...}* {ensures e ...}* {decreases e ...}* { {S; ...}* }
    MDecl Method
  deriving (Eq, Ord, Read)

-- | Back-end program syntax:
--
-- > P ::= {D \n D}*
newtype Program = Program [Decl] deriving (Eq, Ord, Read)

-- | Unparser precedence order helper for binary operations. 
-- Does not wrap sub-tree expressions operations
(<.|.>) :: Exp -> Either Exp Exp -> String
(<.|.>) e1 lre2 =
  let needParens =
        ( case (e1, lre2) of
            (_ :<==> _, Right (_ :<==> _)) -> False
            (_ :<==> _, Left (_ :<==> _)) -> False
            (_ :==> _, Left (_ :==> _)) -> True    -- Need parentheses if left is implication
            (_ :==> _, Right (_ :==> _)) -> False
            (_ :&& _, Right (_ :&& _)) -> False    -- No need for parentheses for AND
            (_ :&& _, Left (_ :&& _)) -> False
            (_ :|| _, Right (_ :|| _)) -> False    -- No need for parentheses for OR
            (_ :|| _, Left (_ :|| _)) -> False
            (_ :&& _, Right (_ :|| _)) -> True     -- Need parentheses if mixed AND and OR
            (_ :&& _, Left (_ :|| _)) -> True
            (_ :|| _, Right (_ :&& _)) -> True
            (_ :|| _, Left (_ :&& _)) -> True
            (_ :== _, Right (_ :== _)) -> True     -- Need parentheses for equality and inequality
            (_ :== _, Left (_ :== _)) -> True
            (_ :== _, Right (_ :!= _)) -> True
            (_ :== _, Left (_ :!= _)) -> True
            (_ :+ _, Right (_ :+ _)) -> False      -- No need for parentheses for addition
            (_ :+ _, Left (_ :+ _)) -> False
            (_ :+ _, Right (_ :- _)) -> False      -- No need for parentheses for addition and subtraction mix
            (_ :+ _, Left (_ :- _)) -> False
            (_ :- _, Right (_ :- _)) -> True       -- Need parentheses if left is subtraction
            (_ :- _, Left (_ :- _)) -> False
            (_ :- _, Left (_ :+ _)) -> False
            (_ :- _, Right (_ :+ _)) -> True
            (_ :* _, Right (_ :* _)) -> False      -- No need for parentheses for multiplication
            (_ :* _, Left (_ :* _)) -> False
            (_ :* _, Right (_ :/ _)) -> False      -- No need for parentheses for multiplication and division mix
            (_ :* _, Left (_ :/ _)) -> False
            (_ :/ _, Right (_ :/ _)) -> True       -- Need parentheses if left is division
            (_ :/ _, Left (_ :/ _)) -> False
            (_ :/ _, Left (_ :* _)) -> False
            (_ :/ _, Right (_ :* _)) -> True
            (_ :% _, Left (_ :% _)) -> True        -- Need parentheses for modulo
            (_ :% _, Right (_ :% _)) -> True
            _ -> e1 > either id id lre2
        )
      trans = (if needParens then ("(" ++) . (++ ")") else id) . prettyPrint 0
   in either trans trans lre2

(<.>) :: Exp -> Exp -> String
(<.>) e1 e2 =
  let needParens =
        ( case (e1, e2) of
            (Not {}, Not {}) -> False
            _ -> e1 > e2
        )
      trans = (if needParens then ("(" ++) . (++ ")") else id) . prettyPrint 0
   in trans e2

instance Show Type where
  show = prettyPrint 0

-- Pretty printer
instance PrettyPrint Type where
  prettyPrint _ =
    let pp = prettyPrint 0
     in \case
          TBad -> "err"
          TInt -> "int"
          TNat -> "nat"
          TBool -> "bool"
          TVar x -> x
          t1 :-> t2 -> concat ["(" ++ pp t1 ++ ")", " -> ", "(" ++ pp t2 ++ ")"]
          Tuple ts -> "(" ++ intercalate ", " (map pp ts) ++ ")"

instance PrettyPrint Pattern where
  prettyPrint _ =
    let pp = prettyPrint 0
     in \case
          Wildcard -> "_"
          PCon c -> prettyPrint 0 c
          PVar x -> x
          PAdt c ps -> c ++ "(" ++ intercalate ", " (map pp ps) ++ ")"
          PTuple ps -> "(" ++ intercalate ", " (map pp ps) ++ ")"

instance PrettyPrint Stmt where
  prettyPrint i s =
    let pp = prettyPrint i
        ind = indent i ""
        ind1 = indent (i + 1) ""
        s' = case s of
          Assign xes ->
            unwords [intercalate ", " (map fst xes), ":=", intercalate ", " (map (prettyPrint 0 . snd) xes) ++ ";"]
          Block ss ->
            if null ss
              then ""
              else
                "{\n"
                  ++ ind1
                  ++ intercalate ("\n" ++ ind1) (map (prettyPrint $ i + 1) ss)
                  ++ "\n"
                  ++ ind
                  ++ "}"
          VarDef g xs ->
            let def (x, mt, e) =
                  let t = case mt of
                        Just t' -> [":", prettyPrint (i + 1) t']
                        Nothing -> []
                   in (unwords $ x : t, prettyPrint (i + 1) e)
                defs = map def xs
                (xs', es') = (intercalate ", " (map fst defs), intercalate ", " (map snd defs))
                g' = (["ghost" | g])
             in unwords (g' ++ ["var", xs', ":=", es' ++ ";"])
          If e s1 ms2 ->
            let s2 = case ms2 of
                  Just s2' -> "\n" ++ ind ++ unwords ["else", pp s2']
                  Nothing -> ""
             in unwords ["if", prettyPrint 0 e, prettyPrint i s1]
                  ++ s2
          Assert e -> unwords ["assert", prettyPrint 0 e] ++ ";"
          MatchStmt e cs ->
            let def (p, s'') = "\n" ++ ind ++ unwords ["case", prettyPrint i p, "=>", prettyPrint (i + 1) s'']
                cs' = map def cs
             in unwords ["match", prettyPrint (i + 1) e, "{"]
                  ++ concat cs'
                  ++ ("\n" ++ ind ++ "}")
          While e es1 es2 s'' ->
            let e' = prettyPrint 0 e
                cons kw e'' = "\n" ++ ind1 ++ unwords [kw, prettyPrint (i + 1) e'']
                es' = concat (map (cons "invariant") es1 ++ map (cons "decreases") es2) ++ " "
             in unwords ["while", e'] ++ es' ++ prettyPrint i s''
          Return es -> unwords ["return", intercalate ", " (map (prettyPrint i) es)]
     in s'

instance PrettyPrint Const where
  prettyPrint _ = \case
    CTrue -> "true"
    CFalse -> "false"
    CNum n -> show n

instance PrettyPrint Exp where
  prettyPrint i e =
    let pp = prettyPrint i
        (tab, ind) = (indent i, indent i "")
        quantifier q xs e' =
          let def (x, mt) =
                let t' = case mt of
                      Just t -> " " ++ prettyPrint i t
                      Nothing -> ""
                 in x ++ t'
              xs' = intercalate ", " $ map def xs
              e'' = pp e'
           in unwords [q, xs', "::", e'']
        bin e1 op e2 = unwords [e <.|.> Left e1, op, e <.|.> Right e2]
        un op e' = unwords [op ++ e <.> e']
     in case e of
          Any -> "*"
          ETuple ps -> "(" ++ intercalate ", " (map pp ps) ++ ")"
          EVar x -> x
          ECon c -> prettyPrint 0 c
          In e1 e2 -> bin e1 "in" e2
          ESet es -> unwords ["{", intercalate ", " $ map (prettyPrint i) es, "}"]
          Exists xs e' -> quantifier "exists" xs e'
          Forall xs e' -> quantifier "forall" xs e'
          e1 :==> e2 -> bin e1 "==>" e2
          e1 :<==> e2 -> bin e1 "<==>" e2
          e1 :&& e2 -> bin e1 "&&" e2
          e1 :|| e2 -> bin e1 "||" e2
          Not e' -> un "!" e'
          e1 :== e2 -> bin e1 "==" e2
          e1 :!= e2 -> bin e1 "!=" e2
          e1 :>= e2 -> bin e1 ">=" e2
          e1 :> e2 -> bin e1 ">" e2
          e1 :<= e2 -> bin e1 "<=" e2
          e1 :< e2 -> bin e1 "<" e2
          e1 :+ e2 -> bin e1 "+" e2
          e1 :- e2 -> bin e1 "-" e2
          e1 :* e2 -> bin e1 "*" e2
          e1 :/ e2 -> bin e1 "/" e2
          e1 :% e2 -> bin e1 "%" e2
          IfElse e1 e2 e3 -> unwords ["if", pp e1, "then", pp e2, "else", pp e3]
          Match e' cs ->
            let def (p, e'') = unwords [tab "case", prettyPrint i p, "=>", pp e'']
                cs' = map def cs
             in unwords ["match", pp e', "{\n"]
                  ++ intercalate ("\n" ++ ind) cs'
                  ++ ("\n" ++ tab "}")
          Call f es -> f ++ "(" ++ intercalate ", " (map pp es) ++ ")"

instance PrettyPrint Cons where
  prettyPrint i (Cons n fs) =
    let fdef (f, t) = unwords [f, ":", prettyPrint i t]
     in n ++ "(" ++ intercalate ", " (map fdef fs) ++ ")"

instance PrettyPrint Function where
  prettyPrint _ Function {yields, funcHoare, funcBody} = case funcHoare of
    HoareWrap {ghost, name, types, params, decreases, requires, ensures} ->
      let ps = intercalate ", " (map (\(x, t) -> unwords [x, ":", prettyPrint 0 t]) params)
          ts =
            if null types
              then ""
              else "<" ++ intercalate ", " (map (prettyPrint 0) types) ++ ">"
          header =
            unwords $
              ["ghost" | ghost]
                ++ [ "function",
                     name ++ ts ++ "(" ++ ps ++ ")",
                     ":",
                     prettyPrint 0 yields
                   ]
          pre = prop "requires" requires
          post = prop "ensures" ensures
          dec = prop "decreases" decreases
          props = intercalate "\n" (pre ++ post ++ dec)
          body = prettyPrint 1 funcBody
          prop kw = map (((indent 1 kw) ++) . prettyPrint 2)
       in intercalate "\n" [header, props ++ "{", body, "}"]

instance PrettyPrint Method where
  prettyPrint _ Method {returns, methodHoare, methodBody} = case methodHoare of
    HoareWrap {ghost, name, types, params, decreases, requires, ensures} ->
      let ps = intercalate ", " (map (\(x, t) -> unwords [x, ":", prettyPrint 0 t]) params)
          ts =
            if null types
              then ""
              else "<" ++ intercalate ", " (map (prettyPrint 0) types) ++ ">"
          rps = map (\(x, t) -> unwords [x, ":", prettyPrint 0 t]) returns
          method = if ghost then "lemma" else "method"
          header = unwords [method, name ++ ts ++ "(" ++ ps ++ ")", "returns", "(" ++ intercalate ", " rps ++ ")"]
          pre = prop "requires" requires
          post = prop "ensures" ensures
          dec = prop "decreases" decreases
          props = intercalate "\n" (pre ++ post ++ dec)
          body = prettyPrint 0 methodBody
          prop kw = map (\e -> indent 1 $ unwords [kw, prettyPrint 2 e])
       in intercalate "\n" [header, props, body]

instance PrettyPrint Decl where
  prettyPrint _ = \case
    Datatype s ts cs ->
      let ts' = intercalate ", " (map (prettyPrint 0) ts)
          cs' = intercalate " | " (map (prettyPrint 0) cs)
       in unwords ["datatype", s, "<" ++ ts' ++ ">", "=", cs']
    TypeDecl x t -> unwords ["type", x, "=", prettyPrint 0 t]
    FDecl f -> prettyPrint 0 f
    MDecl m -> prettyPrint 0 m

instance PrettyPrint Program where
  prettyPrint _ (Program ds) = intercalate "\n\n" (map (prettyPrint 0) ds)
