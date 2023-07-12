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