module Pipeline.Translation.CallgraphOk (noRecursion) where

import Data.Map qualified as M
import Data.Maybe (isJust)
import Data.Set qualified as S
import Pipeline.Callgraph (getCG)
import Promela.Ast
import Promela.Utilities
import Utilities.Position
import Utilities.Err

data Ctxt a = Ctxt
  { ancestors :: S.Set String,
    callgraph :: M.Map String Module,
    visited :: S.Set String,
    syntax :: a
  }
  deriving (Eq, Ord, Read, Show)

makeCtx :: a -> Ctxt a
makeCtx a =
  Ctxt
    { ancestors = S.empty,
      callgraph = M.empty,
      visited = S.empty,
      syntax = a
    }

transferCtx :: a -> Ctxt b -> Ctxt a
transferCtx a = \case
  Ctxt {ancestors, callgraph, visited} ->
    Ctxt
      { ancestors = ancestors,
        callgraph = callgraph,
        visited = visited,
        syntax = a
      }

wrapCtx :: Ctxt b -> Ctxt ()
wrapCtx = transferCtx ()

updateCtxVisited :: Ctxt a -> Ctxt b -> Ctxt a
updateCtxVisited ctx ctx' =
  ctx {visited = S.union (visited ctx) (visited ctx')}

noRecursion :: Spec -> Err ()
noRecursion (Spec ms) =
  let ss = getInit (Spec ms)
      ctx = (makeCtx ss) {callgraph = getCG (Spec ms)}
   in if Data.Maybe.isJust (traverseStmts ctx) then return ()
    else Bad "Found call-graph cycle"

traverseStmts :: Ctxt [Pos Stmt] -> Maybe (Ctxt ())
traverseStmts ctx = case syntax ctx of
  [] -> Just (wrapCtx ctx)
  Pos _ s : ss -> do
    -- Traverse successor statements
    ctx' <- traverseStmts (ctx {syntax = ss})
    -- Traverse lists of syntax sequences.
    let visit traverseF startingCtx =
          Prelude.foldl
            ( \mc syn -> case mc of
                Just c -> fmap (updateCtxVisited c) (traverseF (c {syntax = syn}))
                Nothing -> mc
            )
            (Just startingCtx)
    -- Used in branching control flow constructs.
    let visitBranches = visit traverseStmts
    let visitExpressions = visit traverseExp ctx'
    -- Helper function for branching control flow with option "else" branch
    let branchingFlow os mels = do
          ctx'' <- visitBranches ctx' os
          case mels of
            Just els -> traverseStmts (ctx'' {syntax = els})
            Nothing -> Just ctx'
    case s of
      Decl _ _ me -> do
        e <- me
        traverseExp (ctx' {syntax = e})
      If os mels -> branchingFlow os mels
      Do os mels -> branchingFlow os mels
      For r ss' -> do
        ctx'' <- traverseRange (ctx' {syntax = r})
        visitBranches ctx'' [ss']
      As _ e -> traverseExp (ctx' {syntax = e})
      Assert e -> traverseExp (ctx' {syntax = e})
      Rcv _ es -> visitExpressions es
      Send _ es -> visitExpressions es
      ExpS e -> traverseExp (ctx' {syntax = e})
      Label _ stm -> traverseStmts (ctx' {syntax = [stm]})
      _ -> return ctx'

traverseRange :: Ctxt Range -> Maybe (Ctxt ())
traverseRange ctx =
  let visit =
        Prelude.foldl
          ( \mc e -> case mc of
              Just c -> fmap (updateCtxVisited c) (traverseExp (c {syntax = e}))
              Nothing -> mc
          )
          (Just (wrapCtx ctx))
   in case syntax ctx of
        Between _ e1 e2 -> visit [e1, e2]
        _ -> Just (wrapCtx ctx)

traverseExp :: Ctxt Exp -> Maybe (Ctxt ())
traverseExp ctx =
  let visit =
        Prelude.foldl
          ( \mc e -> case mc of
              Just c -> fmap (updateCtxVisited c) (traverseExp (c {syntax = e}))
              Nothing -> mc
          )
          (Just (wrapCtx ctx))
   in case syntax ctx of
        And e1 e2 -> visit [e1, e2]
        Or e1 e2 -> visit [e1, e2]
        Eq e1 e2 -> visit [e1, e2]
        Ne e1 e2 -> visit [e1, e2]
        Le e1 e2 -> visit [e1, e2]
        Lt e1 e2 -> visit [e1, e2]
        Ge e1 e2 -> visit [e1, e2]
        Gt e1 e2 -> visit [e1, e2]
        Plus e1 e2 -> visit [e1, e2]
        Minus e1 e2 -> visit [e1, e2]
        Mult e1 e2 -> visit [e1, e2]
        Div e1 e2 -> visit [e1, e2]
        Neg e -> visit [e]
        Not e -> visit [e]
        Run f es ->
          if S.member f (ancestors ctx)
            then Nothing
            else
              if S.member f (visited ctx)
                then Just (wrapCtx ctx)
                else do
                  callee <- M.lookup f (callgraph ctx)
                  case callee of
                    Proc _ _ ss -> do
                      ctx' <- visit es
                      let ctx'' = ctx' {ancestors = S.insert f (ancestors ctx')}
                      ctx''' <- traverseStmts (ctx'' {syntax = ss})
                      return (ctx''' {visited = S.insert f (visited ctx''')})
                    _ -> Nothing
        _ -> Just (wrapCtx ctx)
