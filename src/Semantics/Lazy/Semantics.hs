module Semantics.Lazy.Semantics (eval) where

import Semantics.Semantics (EvalStrategy (..))
import qualified Semantics.Semantics as S (EvalStrategy (..), eval)
import Syntax.Syntax (Program)

evalStrategy :: EvalStrategy Maybe
evalStrategy =
  EvalStrategy
    { S.pure = Prelude.pure,
      toMaybe = id,
      evalArgs = Just
    }

eval :: Program -> Integer
eval = S.eval evalStrategy
