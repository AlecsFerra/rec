{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Semantics.Strict.Semantics (eval) where

import Data.Functor.Identity (Identity (Identity, runIdentity), runIdentity)
import Semantics.Semantics (EvalStrategy (..))
import qualified Semantics.Semantics as S (eval)
import Syntax.Syntax (Program)
import Util ((<$$>))

instance EvalStrategy Identity where
  toMaybe = Just . runIdentity
  evalArgs = (Identity <$$>) . sequence

eval :: Program -> Integer
eval = S.eval @Identity
