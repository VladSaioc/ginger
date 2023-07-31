module IR.SanityCheck (sanityCheck, asyncCheck) where

import Control.Monad
import Data.Set qualified as S
import IR.Ast
import Utilities.Err
import Utilities.General

-- The sanity check context bookkeeps variable names
data Ctx = Ctx
  { -- Iteration variables
    ienv :: S.Set String,
    -- Free variables
    fvs :: S.Set String,
    -- Channel variables
    chenv :: S.Set String
  }
  deriving (Eq, Ord, Read)

-- Perform a sanity check on the given IR program
sanityCheck :: 𝑃 -> Err Ctx
sanityCheck (𝑃 chs prc) = do
  -- Create a fresh context
  let ctx =
        Ctx
          { ienv = S.empty,
            fvs = S.empty,
            chenv = S.empty
          }
  -- Perform sanity checks on channel declarations
  ctx' <- foldM sanityCheckChan ctx chs
  foldM sanityCheckStm ctx' prc

-- Perform sanity checks on channel declarations.
-- Remember any encountered free variables and the names of declared channels.
sanityCheckChan :: Ctx -> Chan -> Err Ctx
sanityCheckChan ctx (Chan c e) = do
  -- Make sure there are no duplicate channel declarations.
  _ <- multiGuard [(S.member c (chenv ctx), "Duplicate channel declaration: " ++ c)]
  -- Remember the name of the newly declared channel
  let ctx' = ctx {chenv = S.insert c (chenv ctx)}
  -- Sanity check the capacity expression
  sanityCheckExp ctx' e

-- Perform sanity checks on IR statements.
sanityCheckStm :: Ctx -> 𝑆 -> Err Ctx
sanityCheckStm ctx = \case
  If e s1 s2 -> do
    ctx' <- sanityCheckExp ctx e
    ctx'' <- sanityCheckStm ctx' s1
    sanityCheckStm ctx'' s2
  Skip -> return ctx
  Seq s1 s2 -> do
    ctx' <- sanityCheckStm ctx s1
    sanityCheckStm ctx' s2
  For x e1 e2 os -> do
    ctx' <- sanityCheckExp ctx e1
    ctx'' <- sanityCheckExp ctx' e2
    _ <-
      multiGuard
        -- Ensure there are no duplicate loop variables
        [ (S.member x (ienv ctx), "Duplicate loop variable: " ++ x),
          -- Ensure that a name used by a channel is not reused for a loop index
          (S.member x (chenv ctx), "Channel used as loop index: " ++ x),
          -- Ensure that a free variable is not used as a loop index
          (S.member x (chenv ctx), "Free variable used as loop index: " ++ x)
        ]
    -- Bookkeep loop index variable name
    let ctx''' = ctx'' {ienv = S.insert x (ienv ctx)}
    foldM sanityCheckOp ctx''' os
  Atomic op -> sanityCheckOp ctx op

-- Sanity check a channel operation. Ensures that
-- the channel named by the operation was previously declared.
sanityCheckOp :: Ctx -> Op -> Err Ctx
sanityCheckOp ctx =
  let checkChan c =
        if S.member c (chenv ctx)
          then return ctx
          else Bad ("Usage of undeclared channel: " ++ c)
   in \case
        Send c -> checkChan c
        Recv c -> checkChan c

-- Sanity check IR expressions. Ensures that loop variables or channel
-- names are not used in expressions.
sanityCheckExp :: Ctx -> 𝐸 -> Err Ctx
sanityCheckExp ctx =
  let bin e1 e2 = do
        ctx' <- sanityCheckExp ctx e1
        sanityCheckExp ctx' e2
   in \case
        e1 :& e2 -> bin e1 e2
        e1 :| e2 -> bin e1 e2
        Not e1 -> bin BTrue e1
        e1 :== e2 -> bin e1 e2
        e1 :!= e2 -> bin e1 e2
        e1 :< e2 -> bin e1 e2
        e1 :<= e2 -> bin e1 e2
        e1 :> e2 -> bin e1 e2
        e1 :>= e2 -> bin e1 e2
        e1 :+ e2 -> bin e1 e2
        e1 :- e2 -> bin e1 e2
        e1 :* e2 -> bin e1 e2
        e1 :/ e2 -> bin e1 e2
        Const _ -> return ctx
        BTrue -> return ctx
        BFalse -> return ctx
        Var x -> do
          _ <-
            multiGuard
              [ (S.member x (ienv ctx), "Loop variable used as a free variable: " ++ x),
                (S.member x (chenv ctx), "Channel used as a free variable: " ++ x)
              ]
          return (ctx {fvs = S.insert x (fvs ctx)})

asyncCheck :: 𝑃 -> Err [()]
asyncCheck (𝑃 cs _) = results $ map asyncCheckChan cs

asyncCheckChan :: Chan -> Err ()
asyncCheckChan (Chan c e) = case e of
  Const 0 -> Bad ("Channel " ++ c ++ " is synchronous.")
  _ -> return ()
