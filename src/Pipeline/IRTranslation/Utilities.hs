module Pipeline.IRTranslation.Utilities where

import Backend.Ast
import Control.Monad (unless)
import Data.List (intercalate)
import Data.Map qualified as M
import Data.Maybe
import Data.Set qualified as S
import IR.Utilities
import Utilities.PrettyPrint (PrettyPrint (prettyPrint))

-- An alias for the type of channel names, denoted as strings.
-- Its purpose is to shorten type definitions involving channel names.
type Ch = String

-- Mappings from channel operation directionality to a set of
-- program points marking channel operations with that direction.
type ChOps = M.Map OpDir (S.Set PCounter)

-- Polymorphic mappings from channel names.
type ChMap a = M.Map Ch a

-- Mappings from from process ids to channel operations, organized
-- as maps from channel names to directional program points.
type PChInsns = M.Map Pid (ChMap ChOps)

-- An alias for the type of process ids. Its purpose is to provide
-- clarity to type definitions involving process ids.
type Pid = Int

-- An alias for the type of program points. Its purpose is to provide
-- clarity to type definitions involving program points.
type PCounter = Int

-- Bindings from program points to statements, encoding the semantics
-- of the operation at the given point.
type ProgPoints = M.Map PCounter Stmt

-- Bindings from process ids to program points.
type Procs = M.Map Pid ProgPoints

-- Channel capacity environments, connecting channel names to capacity expressions.
type KEnv = ChMap Exp

-- (Meta)data about loops found in the program.
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
    -- Channel operations in the loop (indexed by channel name)
    chans :: ChMap ChOps
  }

instance Show Loop where
  show Loop {pid, var, guardP, exitP, lower, upper, chans} =
    unlines
      -- PID: for x (lo .. hi)
      [ unwords
          [ show pid ++ ":",
            "for",
            var,
            "(" ++ prettyPrint 0 lower,
            "..",
            prettyPrint 0 upper ++ ")",
            "<" ++ show guardP ++ "-->" ++ show exitP ++ ">"
          ],
        intercalate ", " (M.elems (M.map show chans))
      ]

-- Annotate process-local variable. Given process id pid and name x,
-- the naming schema is:
--  P'pid'x
(%) :: Pid -> String -> String
(%) pid x = "P" ++ show pid ++ "'" ++ x

-- Program counter variable name. Produces the variable storing program
-- counters for each process. Given process id pid, the naming schema is:
--  Ppid
(<|) :: Pid -> String
(<|) pid = "P" ++ show pid

-- Given a set of program points, produces the next available program point.
(-|) :: ProgPoints -> Int
(-|) pp = case M.toDescList pp of
  [] -> 0
  (n, _) : _ -> n

-- Checks that a sequence of values are all equal, by performing pair-wise structural equality.
equals :: Eq a => [a] -> Maybe a
equals = \case
  [] -> Nothing
  [a] -> Just a
  a' : a'' : as ->
    if a' == a''
      then equals (a'' : as)
      else Nothing

-- Checks whether a back-end statement encodes a channel send or receive
-- operation, and returns the name of the channel if that is the case.
-- The result is wrapped in "Left" for channel sends, and "Right" for channel
-- receives. Channel send operations have the following pattern:
--
-- if 0 < κ(c) {
--    if c {< κ(c), > 0} {
--       c := c {+,-} 1;
--       pc := n;
--    }
-- } else {
--    if c == {0, 1} {
--       c := {1, -1};
--       pc := n';
--    }
-- }
-- where n' = {n + 1, n}
backendChannelOp :: Stmt -> Maybe (Either Ch Ch)
backendChannelOp =
  let -- Return a result inside a pair
      result c x = return (c, x)
      -- Results that conform to the "channel send" pattern pair
      -- relevant results with the "Left" constructor.
      send = result Left
      -- Results that conform to the "channel receive" pattern pair
      -- relevant results with the "Right" constructor.
      recv = result Right
      -- Check that the communication case is a block with a single if-statement
      -- without an else case. This is satisfied by both synchronous and
      -- asynchronous communication:
      -- { if guard { body } }
      -- Yields the guard and the body.
      commCase = \case
        Block [If guard body Nothing] -> return (guard, body)
        _ -> Nothing
   in \case
        -- The channel operation must be modelled as a statement that
        -- changes the semantics of the operation based on whether the channel
        -- is buffered or unbuffered.
        --
        -- This requires an if statement of the form:
        -- if 0 < κ(c) { asynchronous case } else { synchronous case }
        If capGuard async (Just sync) -> do
          -- Check that the capacity guard is of the form: 0 < κ(c)
          k <- case capGuard of
            Lt (ECon (CNum 0)) k -> return k
            _ -> Nothing

          -- Get the asynchronous case guard and body.
          (aGuard, aBody) <- commCase async
          -- Check that the asynchronous operation guard is valid.
          -- The produced constructor "d" informs the next steps
          -- what is the direction of the channel operation
          -- ("Left" for send, "Right" for receive).
          --
          -- Produces c: The name of the channel used in the guard.
          (d, c) <- case aGuard of
            -- For send, the guard checks that the channel is not full.
            -- It also checks that the capacity expression is consistent
            -- between the capacity and the operation guards:
            -- c < κ(c)
            Lt (EVar c) k' -> do
              unless (k == k') Nothing
              send c
            -- For receive, the guard checks that the channel is not empty:
            -- c > 0
            Gt (EVar c) (ECon (CNum 0)) -> recv c
            _ -> Nothing
          -- Check that the statement conforms to asynchronous operation encoded.
          -- The constructor "d" produced at the previous step ensures
          -- that the check produces consistent results.
          --
          -- Yields the following:
          -- - p1: the process variable being modified
          -- - n1: the program point of the next operation
          -- - c1: the name of the channel variable on the LHS of the assignment
          -- - c2: the name of the channel variable on the RHS of the assignment
          (d1, (p1, n1, c1, c2)) <- case d aBody of
            -- For send, the semantics is encoded as incrementing the
            -- channel buffer length, and progressing to the next operation.
            Left
              ( Block
                  [ Assign [(c1, Plus (EVar c2) (ECon (CNum 1)))],
                    Assign [(p1, ECon (CNum n1))]
                    ]
                ) -> send (p1, n1, c1, c2)
            -- For receive, the semantics is encoded as decrementing the
            -- channel buffer length, and progressing to the next operation.
            Right
              ( Block
                  [ Assign [(c1, Minus (EVar c2) (ECon (CNum 1)))],
                    Assign [(p1, ECon (CNum n1))]
                    ]
                ) -> recv (p1, n1, c1, c2)
            _ -> Nothing

          -- Get the synchronous case guard and body.
          (sGuard, sBody) <- commCase sync
          -- Check that the synchronous operation guard is valid.
          -- The produced constructor "d2" informs the next steps
          -- what is the direction of the channel operation
          -- ("Left" for send, "Right" for receive).
          --
          -- Produces c3: The name of the channel used in the guard.
          (d2, c3) <- case d1 sGuard of
            -- For send, the guard checks that the channel is ready to send
            -- (channel value is encoded as 0):
            -- c == 0
            Left (Eq (EVar c3) (ECon (CNum 0))) -> send c3
            -- For receive, the guard checks that the channel is ready to
            -- synchronize (channel value is encoded as 1):
            -- c == 1
            Right (Eq (EVar c3) (ECon (CNum 1))) -> recv c3
            _ -> Nothing
          -- Check that the statement conforms to asynchronous operation encoded.
          -- The constructor "d" produced at the previous step ensures
          -- that the check produces consistent results.
          --
          -- Yields the following:
          -- - p2: the process variable being modified
          -- - n2: the program point of the next operation
          -- - c4: the name of the channel variable on the LHS of the assignment
          (dfinal, (p2, n2, c4)) <- case d2 sBody of
            -- For send, the semantics is encoded as incrementing the
            -- channel buffer length, and progressing to the next operation.
            -- The return program point is offset by 1, because the immediate next point
            -- is occupied by the synchronization 'rendezvous' operation.
            Left
              ( Block
                  [ Assign [(c4, ECon (CNum 1))],
                    Assign [(p2, ECon (CNum n2))]
                    ]
                ) -> send (p2, n2 + 1, c4)
            Right
              ( Block
                  [ Assign [(c4, ECon (CNum (-1)))],
                    Assign [(p2, ECon (CNum n2))]
                    ]
                ) -> recv (p2, n2, c4)
            _ -> Nothing
          -- Check that the process name is consistent between the
          -- synchronous and asynchronous cases.
          unless (p1 == p2) Nothing
          -- Check that the switch to the next operation is consistent
          -- between the two cases.
          unless (n1 == n2) Nothing
          -- Check that the channel names used throughout the operation
          -- are consistent.
          c' <- equals [c, c1, c2, c3, c4]
          -- Return the channel name, wrapped in "Left" if the operation
          -- is a send, or "Right" if the operation is a receive.
          return (dfinal c')
        -- The statement does not conform to any channel operation pattern.
        _ -> Nothing

-- Point-wise binary union between maps of channel operations.
-- Given M1 and M2, it produces:
--
-- [ c ↦ [d ↦ M1(c)(d) ∪ M1(c)(d) | d ∈ {!, ?}] | c ∈ dom(M1) ∪ dom(M2) ]
(⊎) :: Ord a => Ord b => Ord c => M.Map a (M.Map b (S.Set c)) -> M.Map a (M.Map b (S.Set c)) -> M.Map a (M.Map b (S.Set c))
(⊎) = M.unionWith $ M.unionWith S.union

-- Inserts a channel operation into a channel operation map.
-- Given, a triple (c, d, n) where c is a channel name, d
-- is the direction of an operation, and n is the program point
-- of the operation, and a map of channel operations M,
-- the result is:
--
-- M[c ↦ M(c)[d ↦ M(c)(d) ∪ {n}]]
--
-- If M(c) is undefined (and similarly M(c)(d)), they get initialized
-- to the corresponding zero value for the appropriate type.
(+>) :: (Ch, OpDir, PCounter) -> ChMap ChOps -> ChMap ChOps
(c, d, n) +> chops =
  let ops = fromMaybe M.empty (M.lookup c chops)
      dops = fromMaybe S.empty (M.lookup d ops)
      dops' = S.insert n dops
      ops' = M.insert d dops' ops
   in M.insert c ops' chops