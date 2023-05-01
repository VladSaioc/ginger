module Promela.Utilities (getInit) where

import Data.List qualified as L
import Promela.Ast
import Utilities.Position

getInit :: Spec -> [Pos Stmt]
getInit (Spec ms) =
  let i =
        L.find
          ( \case
              Init {} -> True
              _ -> False
          )
          ms
   in case i of
        Just (Init ss) -> ss
        _ -> []