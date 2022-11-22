{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module Semantics.Lazy.Semantics (eval) where

import Semantics.Semantics (EvalStrategy (..))
import qualified Semantics.Semantics as S (eval)
import Syntax.Syntax (Program)

instance EvalStrategy Maybe where
  toMaybe = id
  evalArgs = Just

eval :: Program -> Integer
eval = S.eval @Maybe
