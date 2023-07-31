module Go.Topology where

import Go.Ast qualified as G

data TopoNType
  = Spawn
  | Internal
  | Done
  | Send String
  | Recv String
  | Close String
  | Choice
  | Loop String
  deriving (Eq, Ord, Read)

data TopoNode = TopoNode String TopoNType
  deriving (Eq, Ord, Show, Read)

instance Show TopoNType where
  show = \case
    Spawn -> "go"
    Internal -> "τ"
    Done -> "."
    Send c -> c ++ "!"
    Recv c -> c ++ "?"
    Close c -> "[" ++ c ++ "]"
    Choice -> "⨁"
    Loop x -> "<" ++ x ++ ">"
