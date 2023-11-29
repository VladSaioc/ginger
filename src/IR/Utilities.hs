module IR.Utilities where

import IR.Ast

-- | Short-hand type of channel operation.
data CommOpType = S | R deriving (Eq, Ord, Read)
-- | Short-hand type of WaitGroup operation
data WgOpType = A | W deriving (Eq, Ord, Read)
-- | Short-hand type for any concurrency operation
data OpType = CommO CommOpType | WgO WgOpType deriving (Eq, Ord, Read)

-- | Get concurrency primitive name.
primName :: Op -> String
primName = \case
  Send c -> c
  Recv c -> c
  Wait w -> w
  Add w _ -> w

-- | Get concurrent operation type from instruction
opType :: Op -> OpType
opType = \case
  Send _ -> CommO S
  Recv _ -> CommO R
  Wait _ -> WgO W
  Add _ _ -> WgO A

instance Show CommOpType where
  show = \case
    S -> "!"
    R -> "?"

instance Show WgOpType where
  show = \case
    A -> "Add"
    W -> "Wait"

instance Show OpType where
  show = \case
    CommO o -> show o
    WgO o -> show o

-- | Checks whether the IR program is interesting.
-- If the program is not interesting, no back-end code is emitted.
--
-- > [SEND]:      interesting(c!)
-- > [RECV]:      interesting(c?)
-- > [WAIT]:      interesting(w.Wait())
-- > [ADD]:       interesting(w.Add(e))
-- > [SEQ-1]:     interesting(S1; S2)
-- >              |- interesting(S1)
-- > [SEQ-2]:     interesting(S1; S2)
-- >              |- interesting(S2)
-- > [IF-1]:      interesting(if e then S1 else S2)
-- >              |- interesting(S1)
-- > [IF-2]:      interesting(if e then S1 else S2)
-- >              |- interesting(S2)
-- > [FOR]:       interesting(for x : e1 .. e2 { S })
-- >              |- interesting(S)
interesting :: 𝑃 -> Bool
interesting (𝑃 _ s) = interestingStmt s

-- | Checks whether a VIRGo statement is interesting.
interestingStmt :: 𝑆 -> Bool
interestingStmt =
  let
    bin s1 s2 = interestingStmt s1 || interestingStmt s2
  in \case
    Atomic _ -> True
    Seq s1 s2 -> bin s1 s2
    If _ s1 s2 -> bin s1 s2
    For _ _ _ os -> not (null os)
    Go s1 -> interestingStmt s1
    _ -> False
