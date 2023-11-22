module Pipeline.IRTranslation.Clauses.Utilities where

import Backend.Ast

-- | Creates a call to the "iter" function on loop bounds.
--
-- Produces:
--
-- > iter(lo,hi)
iterations :: Exp -> Exp -> Exp
iterations lo hi = Call "iter" [lo, hi]
