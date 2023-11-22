module IR.Ast where

import Utilities.PrettyPrint (PrettyPrint (prettyPrint), indent, multiline)

-- | Every syntactical construct that may be converted to program
-- points must implement program point offset. Computes the offset required,
-- in terms of program points, to reach the next instruction.
--
-- The offsets are:
--
-- * For Statements
-- 1. skip: 0 (skip statements are ignored)
-- 2. return: 1 for the return instruction point
-- 3. 𝑆₁; 𝑆₂: |𝑆₁| + |𝑆₂|
-- 4. for x : 𝐸₁ .. 𝐸₂ { 𝑠 }: 2 + |𝑠|
--      1 for the guard
--      1 for the index incrementing operation
-- 5. if 𝐸 𝑆₁ 𝑆₂ -> 2 + |𝑆₁| + |𝑆₂|
--      1 for the guard
--      1 for the continuation of the 'then' path
--
-- * For channel operations:
-- 1. 𝑐!: 2 (send + rendezvous)
-- 2. 𝑐?: 1 (receive)
class ProgramPointOffset a where
  ppOffset :: a -> Int

-- | Production rule for VIRGo programs:
-- > 𝑃 ::= {𝐷; ...}* {go { 𝑆 } ...}*
data 𝑃 = 𝑃 [𝐷] 𝑆 deriving (Eq, Ord, Read)

-- | Production rules for VIRGo definitions:
-- > 𝐷 ::= c = [e]
-- >  | x = sync.WaitGroup
data 𝐷
  = -- | > c = [e]
    Chan String 𝐸
  | -- | > x = sync.WaitGroup
    Wg String
  deriving (Eq, Ord, Read)

-- | Production rule for VIRGo statements:
-- > 𝑆 ::= 𝑆₁; 𝑆₂
-- >  | if 𝐸 then 𝑆₁ else 𝑆₂
-- >  | skip
-- >  | return
-- >  | for (x : 𝐸₁ .. 𝐸₂) { 𝑠 }
-- >  | 𝑐! | 𝑐? | 𝑤.Add(𝐸) | 𝑤.Wait()
data 𝑆
  = -- | > 𝑆₁; 𝑆₂
    Seq 𝑆 𝑆
  | -- | > if 𝐸 then 𝑆₁ else 𝑆₂
    If 𝐸 𝑆 𝑆
  | -- | > skip
    Skip
  | -- | > return
    Return
  | -- | > for (x : 𝐸₁ .. 𝐸₂) { 𝑠 }
    For String 𝐸 𝐸 [Op]
  | -- | > go { S }
    Go 𝑆
  | -- | > 𝑐! | 𝑐? | 𝑤.Add(𝐸) | 𝑤.Wait()
    Atomic Op
  deriving (Eq, Ord, Read)

-- | Production rules for loop IR operations
--  > 𝑠 ::= 𝑠₁; 𝑠₂ | 𝑐! | 𝑐?
data Op
  = -- | > 𝑐!
    Send String
  | -- | > 𝑐?
    Recv String
  | -- | > 𝑤.Add(𝐸)
    Add String 𝐸
  | -- | > 𝑤.Wait()
    Wait String
  deriving (Eq, Ord, Read)

-- | Production rules for IR expressions
-- > 𝐸 ::= 𝐸₁ && 𝐸₂
-- >    | 𝐸₁ || 𝐸₂
-- >    | !𝐸
-- >    | 𝐸₁ == 𝐸₂
-- >    | 𝐸₁ != 𝐸₂
-- >    | 𝐸₁ >= 𝐸₂
-- >    | 𝐸₁ + 𝐸₂
-- >    | 𝐸₁ - 𝐸₂
-- >    | 𝐸₁ * 𝐸₂
-- >    | 𝐸₁ / 𝐸₂
-- >    | 𝐸₁ + 𝐸₂
-- >    | true | false
-- >    | 𝑛 | x
data 𝐸
  = -- | 𝐸₁ && 𝐸₂
    𝐸 :& 𝐸
  | -- | 𝐸₁ || 𝐸₂
    𝐸 :| 𝐸
  | -- | !𝐸
    Not 𝐸
  | -- | 𝐸₁ == 𝐸₂
    𝐸 :== 𝐸
  | -- | 𝐸₁ != 𝐸₂
    𝐸 :!= 𝐸
  | -- | 𝐸₁ >= 𝐸₂
    𝐸 :>= 𝐸
  | -- | 𝐸₁ > 𝐸₂
    𝐸 :> 𝐸
  | -- | 𝐸₁ <= 𝐸₂
    𝐸 :<= 𝐸
  | -- | 𝐸₁ < 𝐸₂
    𝐸 :< 𝐸
  | -- | 𝐸₁ + 𝐸₂
    𝐸 :+ 𝐸
  | -- | 𝐸₁ - 𝐸₂
    𝐸 :- 𝐸
  | -- | 𝐸₁ * 𝐸₂
    𝐸 :* 𝐸
  | -- | 𝐸₁ / 𝐸₂
    𝐸 :/ 𝐸
  | -- | 𝑛 ∈ ℤ
    Const Int
  | -- | > true
    BTrue
  | -- | > false
    BFalse
  | -- | x
    Var String
  deriving (Eq, Ord, Read)

instance Show 𝑃 where
  show (𝑃 cs s) =
    let cs' = multiline (map show cs)
     in unlines [cs', prettyPrint 0 s]

instance Show 𝐷 where
  show = \case
    Chan c e -> unwords [c, "=", "[" ++ show e ++ "];"]
    Wg x -> unwords [x, "=", "sync.WaitGroup"]

instance Show 𝑆 where
  show = prettyPrint 0

instance PrettyPrint 𝑆 where
  prettyPrint n =
    let tab = indent n
    in \case
      Seq s1 s2 -> multiline [prettyPrint n s1 ++ ";", prettyPrint n s2]
      Skip -> tab "skip"
      Return -> tab "return"
      Atomic o -> tab $ show o
      If e s1 s2 ->
        multiline
          [ unwords [tab "if", show e, "{"],
            prettyPrint (n + 1) s1,
            unwords [tab "} else {"],
            prettyPrint (n + 1) s2,
            tab "}"
          ]
      For x e1 e2 os ->
        multiline
          [ unwords [tab "for", x, ":", show e1, "..", show e2, "{"],
            multiline $ map (indent (n + 1) . (++ ";") . show) os,
            tab  "}"
          ]
      Go s ->
        multiline
          [ tab "go {",
            prettyPrint (n + 1) s,
            tab "}"
          ]

instance Show 𝐸 where
  show =
    let bin e1 op e2 = unwords ["(" ++ show e1 ++ ")", op, "(" ++ show e2 ++ ")"]
        un op e = unwords [op, "(" ++ show e ++ ")"]
     in \case
          e1 :& e2 -> bin e1 "&" e2
          e1 :| e2 -> bin e1 "|" e2
          Not e -> un "!" e
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
          Const n -> show n
          BTrue -> "true"
          BFalse -> "false"
          Var x -> x

instance Show Op where
  show = \case
    Send c -> c ++ "!"
    Recv c -> c ++ "?"
    Add w e -> w ++ ".Add(" ++ show e ++ ")"
    Wait w -> w ++ ".Wait()"

instance ProgramPointOffset 𝑃 where
  ppOffset (𝑃 _ s) = ppOffset s

-- Computes the offset required, in terms of program points, to reach
-- the instruction following the channel operation, based on its
-- direction.
--
-- The offsets are:
-- 1. skip: 0 (skip statements are ignored)
-- 2. return: 1 for the return instruction point
-- 3. 𝑆₁; 𝑆₂: |𝑆₁| + |𝑆₂|
-- 4. for x : 𝐸₁ .. 𝐸₂ { 𝑠 }: 2 + |𝑠|
--      1 for the guard
--      1 for the index incrementing operation
-- 5. if 𝐸 { 𝑆₁ } else { 𝑆₂ }: 2 + |𝑆₁| + |𝑆₂|
--      1 for the guard
--      1 for the continuation of the 'then' path
-- 6. go { 𝑆 }: 1 for the start goroutine instruction.
instance ProgramPointOffset 𝑆 where
  ppOffset = \case
    Skip -> 0
    Return -> 1
    Seq s1 s2 -> ppOffset s1 + ppOffset s2
    For _ _ _ os -> 2 + sum (map ppOffset os)
    If _ s1 s2 -> 2 + ppOffset s1 + ppOffset s2
    Go _ -> 1
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
    Add _ _ -> 1
    Wait _ -> 1
