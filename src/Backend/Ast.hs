module Backend.Ast where

import Data.List (intercalate)

import Utilities.PrettyPrint

-- | Back-end type syntax:
--
-- > 𝑇 ::= int | nat | bool | 𝑥 | set<𝑇> | 𝑇 -> 𝑇 | ({𝑇, ...}*)
data Type
  = TBad
  | -- int
    TInt
  | -- nat
    TNat
  | -- bool
    TBool
  |  -- set<𝑇>
    TSet Type
  | -- 𝑇
    TVar String
  | -- 𝑇 -> 𝑇
    Type :-> Type
  | -- (𝑇, ...)
    Tuple [Type]
  deriving (Eq, Ord, Read)

-- | Back-end pattern matching syntax:
--
-- > 𝑃 ::= _ | 𝑐 | 𝑥 | C(𝑃, ...) | (𝑃, ...)
data Pattern
  = -- | >  _
    Wildcard
  | -- | > 𝑐
    PCon Const
  | -- | > 𝑥
    PVar String
  | -- | > C(𝑃, ...)
    PAdt String [Pattern]
  | -- | > (𝑃, ...)
    PTuple [Pattern]
  deriving (Eq, Ord, Show, Read)

-- | Back-end statement syntax:
--
-- > 𝑆 ::= {x, ...}* := {𝐸, ...}*
-- >    | { 𝑆; ... }
-- >    | [ghost] var {x [: 𝑇], ...}* := {𝐸, ...}*
-- >    | if 𝐸 { 𝑆 } else { 𝑆 }
-- >    | assert 𝐸
-- >    | match 𝐸 { {case p => 𝑆 ...}* }
-- >    | while 𝐸 {invariant 𝐸 ...}* {decreases 𝐸}* { 𝑆 }
-- >    | return {𝐸, ...}*
data Stmt
  = -- | > {𝑥, ...}* := {𝐸, ...}*
    Assign [(String, Exp)]
  | -- | > { 𝑆; ... }
    Block [Stmt]
  | -- | > [ghost] var {x [: 𝑇], ...}* := {𝐸, ...}*
    VarDef Bool [(String, Maybe Type, Exp)]
  | -- | > if 𝐸 { 𝑆 } else { 𝑆 }
    If Exp Stmt (Maybe Stmt)
  | -- | > assert 𝐸
    Assert Exp
  | -- | > match 𝐸 { {case 𝑃 => 𝑆 ...}* }
    MatchStmt Exp [(Pattern, Stmt)]
  | -- | > while 𝐸 {invariant 𝐸 ...}* {decreases 𝐸}* { 𝑆 }
    While Exp [Exp] [Exp] Stmt
  | -- | > return {𝐸, ...}*
    Return [Exp]
  deriving (Eq, Ord, Read)

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
-- > 𝐸 ::= match 𝐸 { {case p => 𝐸 ...}* }
-- >    | if 𝐸 then 𝐸 else 𝐸
-- >    | exists {x [: 𝑇], ...}* :: 𝐸
-- >    | forall {x [: 𝑇], ...}* :: 𝐸
-- >    | 𝐸 in 𝐸
-- >    | {{𝐸, ...} *}
-- >    | 𝐸 <==> 𝐸 | 𝐸 ==> 𝐸
-- >    | 𝐸 && 𝐸 | 𝐸 || 𝐸
-- >    | 𝐸 == 𝐸 | 𝐸 != 𝐸
-- >    | 𝐸 >= 𝐸 | 𝐸 > 𝐸
-- >    | 𝐸 <= 𝐸 | 𝐸 < 𝐸
-- >    | exists {x [: 𝑇], ...}* :: 𝐸
data Exp
  = -- COMPOUND EXPRESSIONS
    -- | > match 𝐸 { {case p => 𝐸 ...}* }
    Match Exp [(Pattern, Exp)]
  | -- | > if 𝐸 then 𝐸 else 𝐸
    IfElse Exp Exp Exp
  | -- PROPOSITIONAL QUANTIFIERS
    -- | > exists {x [: 𝑇], ...}* :: 𝐸
    Exists [(String, Maybe Type)] Exp
  | -- | > forall {x [: 𝑇], ...}* :: 𝐸
    Forall [(String, Maybe Type)] Exp
  | -- | > 𝐸 in 𝐸
    In Exp Exp
  | -- | > {{𝐸, ...} *}
    ESet [Exp]
  | -- BINARY OPERATORS
    -- Propositional logic
    -- | > 𝐸 <==> 𝐸
    Exp :<==> Exp
  | -- | > 𝐸 ==> 𝐸
    Exp :==> Exp
  | -- Boolean arithmetic
    -- | > 𝐸 && 𝐸
    Exp :&& Exp
  | -- | > 𝐸 || 𝐸
    Exp :|| Exp
  | -- Comparison
    -- | > 𝐸 == 𝐸
    Exp :== Exp
  | -- | > 𝐸 != 𝐸
    Exp :!= Exp
  | -- | > 𝐸 >= 𝐸
    Exp :>= Exp
  | -- | > 𝐸 > 𝐸
    Exp :> Exp
  | -- | > 𝐸 <= 𝐸
    Exp :<= Exp
  | -- | > 𝐸 < 𝐸
    Exp :< Exp
  | -- Arithmetic
    -- | > 𝐸 + 𝐸
    Exp :+ Exp
  | -- | > 𝐸 - 𝐸
    Exp :- Exp
  | -- | > 𝐸 * 𝐸
    Exp :* Exp
  | -- | > 𝐸 / 𝐸
    Exp :/ Exp
  | -- | > 𝐸 % 𝐸
    Exp :% Exp
  | -- UNARY OPERATORS
    -- | > !𝐸
    Not Exp
  | -- | > (𝐸, ... 𝐸)
    ETuple [Exp]
  | -- TERMINAL EXPRESSIONS
    -- | > *
    Any
  | -- | > x
    EVar String
  | -- | > c
    ECon Const
  | -- | > f({𝐸, ...}*)
    Call String [Exp]
  deriving (Eq, Ord, Read)

-- | Back-end record type definition:
--
-- > 𝑇({field : type, ...}*)
data Cons = Cons String [(String, Type)] deriving (Eq, Ord, Read)

-- | Back-end Hoare triple syntax. Uses holes for keyword and return type:
--
-- > H<_, _> ::= _ 𝑥[\<{𝑇, ...}>]({𝑥 : 𝑇, ...}*) _
-- >      {requires 𝐸\n...}*
-- >      {ensures 𝐸\n...}*
-- >      {decreases 𝐸\n...}*
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
-- > F ::= H<[ghost] function, : 𝑇> 𝐸
data Function = Function
  { yields :: Type,
    funcHoare :: HoareWrap,
    funcBody :: Exp
  }
  deriving (Eq, Ord, Read)

-- | Method declaration syntax:
--
-- > M ::= H<lemma | method, returns ({ 𝑥 : 𝑇, ...}*)> { {𝑆; ...}* }
data Method = Method
  { methodReturns :: [(String, Type)],
    methodHoare :: HoareWrap,
    methodBody :: Stmt
  }
  deriving (Eq, Ord, Read)

-- | Back-end top-level declaration:
--
-- > 𝐷 ::= datatype 𝑥<{𝑇, ...}> = {Cons | ...}
-- >    | const 𝑥 := 𝐸
-- >    | type 𝑥 = 𝑇
-- >    | [ghost] function f({𝑥 : 𝑇, ...}*) : 𝑇 {requires 𝐸}* {ensures 𝐸}* { 𝐸 }
-- >    | (lemma | method) function f({𝑥 : 𝑇, ...}*) : 𝑇 {requires 𝐸}* {ensures 𝐸}* { 𝐸 }
data Decl
  = -- | > datatype 𝑥<{𝑇, ...}> = {Cons | ...}
    Datatype String [Type] [Cons]
  | -- | > const 𝑥 := 𝐸
    CDecl String Exp
  | -- | > type 𝑥 = Type
    TypeDecl String Type
  | -- [ghost] function f({𝑥 : 𝑇, ...}*) : 𝑇 {requires 𝐸}* {ensures 𝐸}* { 𝐸 }
    FDecl Function
  | -- (lemma | method) f({𝑥 : 𝑇, ...}*) returns ({𝑥 : 𝑇, ...}*)  {requires 𝐸 ...}* {ensures 𝐸 ...}* {decreases 𝐸 ...}* { {𝑆; ...}* }
    MDecl Method
  deriving (Eq, Ord, Read)

-- | Back-end program syntax:
--
-- > P ::= {𝐷\n 𝐷}*
newtype Program = Program [Decl] deriving (Eq, Ord, Read)

-- | Unparser precedence order helper for binary operations.
-- Does not wrap sub-tree expressions operations
(<.|.>) :: (Exp -> String) -> Exp -> Either Exp Exp -> String
(<.|.>) f e1 lre2 =
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
      trans = (if needParens then ("(" ++) . (++ ")") else id) . f
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

instance Show Cons where
  show = prettyPrint 0

instance Show Exp where
  show = prettyPrint 0

instance Show Program where
  show = prettyPrint 0

instance Show Method where
  show = prettyPrint 0

instance Show Function where
  show = prettyPrint 0

instance Show Stmt where
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
          TSet t -> "set<" ++ pp t ++ ">"
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
                  let t = maybe [] (\t' -> [ ":", prettyPrint (i + 1) t']) mt
                   in (unwords $ x : t, prettyPrint (i + 1) e)
                defs = map def xs
                (xs', es') = (intercalate ", " (map fst defs), intercalate ", " (map snd defs))
                g' = (["ghost" | g])
             in unwords (g' ++ ["var", xs', ":=", es' ++ ";"])
          If e s1 ms2 ->
            let s2 = maybe "" (\s2' -> "\n" ++ ind ++ unwords ["else", pp s2']) ms2
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
          Return es -> unwords ["return", intercalate ", " (map (prettyPrint i) es)] ++ ";"
     in s'

instance PrettyPrint Const where
  prettyPrint _ = \case
    CTrue -> "true"
    CFalse -> "false"
    CNum n -> show n

instance PrettyPrint Exp where
  prettyPrint i e =
    let pp = prettyPrint i
        tab = indent i
        quantifier q xs e' =
          let def (x, mt) =
                let t' = maybe "" ((" : " ++) . prettyPrint i) mt
                 in x ++ t'
              xs' = intercalate ", " $ map def xs
              e'' = pp e'
           in unwords [q, xs', "::", e'']
        bin e1 op e2 = unwords [(<.|.>) pp e (Left e1), op, (<.|.>) pp e (Right e2)]
        un op e' = unwords [op ++ e <.> e']
     in case e of
          Any -> "*"
          ETuple ps -> "(" ++ intercalate ", " (map pp ps) ++ ")"
          EVar x -> x
          ECon c -> prettyPrint 0 c
          In e1 e2 -> bin e1 "in" e2
          ESet es -> unwords ["{", intercalate ", " $ map pp es, "}"]
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
                  ++ unlines cs'
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
          prop kw = map (\e -> indent 1 $ unwords [kw, prettyPrint 2 e])
       in unlines [header, props ++ " {", body, "}"]

instance PrettyPrint Method where
  prettyPrint _ Method {methodReturns, methodHoare, methodBody} = case methodHoare of
    HoareWrap {ghost, name, types, params, decreases, requires, ensures} ->
      let ps = intercalate ", " (map (\(x, t) -> unwords [x, ":", prettyPrint 0 t]) params)
          ts =
            if null types
              then ""
              else "<" ++ intercalate ", " (map (prettyPrint 0) types) ++ ">"
          rps = map (\(x, t) -> unwords [x, ":", prettyPrint 0 t]) methodReturns
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
      let ts' = intercalate ", " (map show ts)
          cs' = intercalate " | " (map show cs)
       in unwords ["datatype", s, "<" ++ ts' ++ ">", "=", cs']
    CDecl x e -> unwords ["const", x, ":=", show e]
    TypeDecl x t -> unwords ["type", x, "=", prettyPrint 0 t]
    FDecl f -> prettyPrint 0 f
    MDecl m -> prettyPrint 0 m

instance PrettyPrint Program where
  prettyPrint _ (Program ds) = intercalate "\n\n" (map (prettyPrint 0) ds)
