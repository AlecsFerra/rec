module Semantics.Lazy.Semantics (eval) where

import Semantics.Semantics (EvalStrategy (..))
import qualified Semantics.Semantics as S (eval)
import Syntax.Syntax (Program)

eval :: Program -> Integer
eval = S.eval strategy

strategy :: EvalStrategy Maybe
strategy =
  EvalStrategy
    { toMaybe = id,
      evalArgs = Just
    }
