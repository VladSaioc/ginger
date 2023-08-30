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

interesting :: 𝑃 -> Bool
interesting (𝑃 _ ps) =
  let
    bin s1 s2 = interestingStmt s1 || interestingStmt s2
    interestingStmt = \case
        Atomic _ -> True
        Seq s1 s2 -> bin s1 s2
        If _ s1 s2 -> bin s1 s2
        For _ _ _ os -> not (null os)
        _ -> False
  in any interestingStmt ps
