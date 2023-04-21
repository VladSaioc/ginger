module Promela.Utilities (getInit) where

import Data.List qualified as L
import Promela.Ast

getInit :: Spec -> Maybe Module
getInit (Spec ms) =
  L.find
    ( \case
        Init {} -> True
        _ -> False
    )
    ms