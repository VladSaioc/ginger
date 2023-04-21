module Pipeline.IRTranslation.Utilities where

import Backend.Ast
import Data.Map qualified as M
import Data.Maybe
import Data.Set qualified as S
import IR.Utilities

type Ch = String

-- Channel resource mapping directionality to a set of
-- program counters marking operations with that direction.
type ChOps = M.Map OpDir (S.Set PCounter)

type ChMap a = M.Map String a

type PChInsns = M.Map Pid (ChMap ChOps)

type Pid = Int

type PCounter = Int

type ProgPoints = M.Map PCounter Stmt

type Procs = M.Map Pid ProgPoints

type KEnv = ChMap Exp

data Loop = Loop
  { -- Process
    pid :: Pid,
    -- Index variable
    var :: String,
    -- Guard program point
    guardP :: PCounter,
    -- Exit program point
    exitP :: PCounter,
    -- Lower bound
    lower :: Exp,
    -- Upper bound
    upper :: Exp,
    -- Number of channel operations in loop (indexed by channel name)
    chans :: ChMap ChOps
  }

-- Annotate process variable
(%) :: Pid -> String -> String
(%) pid x = "P" ++ show pid ++ "'" ++ x

-- Program counter variable name
(<|) :: Pid -> String
(<|) pid = "P" ++ show pid

(-|) :: ProgPoints -> Int
(-|) pp = case M.toDescList pp of
  [] -> 0
  (n, _) : _ -> n

(!?) :: Stmt -> (Bool, String)
(!?) = \case
  If
    (Lt (EVar c) _)
    ( Block
        [ Assign [(c', Plus (EVar c'') (ECon (CNum 1)))],
          Assign [(_, ECon (CNum _))]
          ]
      )
    Nothing -> (c == c' && c' == c'', c)
  _ -> (False, "")

(??) :: Stmt -> (Bool, String)
(??) = \case
  If
    (Gt (EVar c) (ECon (CNum 0)))
    ( Block
        [ Assign [(c', Minus (EVar c'') (ECon (CNum 1)))],
          Assign [(_, ECon (CNum _))]
          ]
      )
    Nothing -> (c == c' && c' == c'', c)
  _ -> (False, "")

(|+|) :: ChMap ChOps -> ChMap ChOps -> ChMap ChOps
(|+|) = M.unionWith (M.unionWith S.union)

(+>) :: ChMap ChOps -> (Ch, OpDir, PCounter) -> ChMap ChOps
chops +> (c, d, n) =
  let ops = fromMaybe M.empty (M.lookup c chops)
      dops = fromMaybe S.empty (M.lookup d ops)
      dops' = S.insert n dops
      ops' = M.insert d dops' ops
   in M.insert c ops' chops