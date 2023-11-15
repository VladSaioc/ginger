module IR.Utilities where

import IR.Ast

data OpDir = S | R deriving (Eq, Ord, Read)

chName :: Op -> String
chName = \case
  Send c -> c
  Recv c -> c

chDir :: Op -> OpDir
chDir = \case
  Send _ -> S
  Recv _ -> R

instance Show OpDir where
  show = \case
    S -> "!"
    R -> "?"

-- | Checks whether the IR program is interesting.
-- If the program is not interesting, the generated back-end code is not emitted.
--
-- > [SEND]:      interesting(c!)
-- > [RECV]:      interesting(c?)
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
