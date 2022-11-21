module Semantics.Strict.Semantics (eval) where

import Data.Functor.Identity (Identity (Identity, runIdentity), runIdentity)
import Semantics.Semantics (EvalStrategy (..))
import qualified Semantics.Semantics as S (eval)
import Syntax.Syntax (Program)
import Util ((<$$>))

eval :: Program -> Integer
eval = S.eval strategy

strategy :: EvalStrategy Identity
strategy =
  EvalStrategy
    { toMaybe = Just . runIdentity,
      evalArgs = (Identity <$$>) . sequence
    }
