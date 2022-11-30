module Semantics.Strict.Semantics (eval) where

import Data.Functor.Identity (Identity (Identity, runIdentity), runIdentity)
import Semantics.Semantics (EvalStrategy (..))
import qualified Semantics.Semantics as S (EvalStrategy (..), eval)
import Syntax.Syntax (Program)
import Util ((<$$>))

evalStrategy :: EvalStrategy Identity
evalStrategy =
  EvalStrategy
    { S.pure = Prelude.pure,
      toMaybe = Just . runIdentity,
      evalArgs = (Identity <$$>) . sequence
    }

eval :: Program -> Integer
eval = S.eval evalStrategy
