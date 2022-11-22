{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeApplications #-}

module Semantics.Lazy.Semantics (eval) where

import Semantics.Semantics (EvalStrategy (..))
import qualified Semantics.Semantics as S (eval)
import Syntax.Syntax (Program)

newtype May a = May
  { runMay :: Maybe a
  }
  deriving (Functor, Applicative)

instance EvalStrategy May where
  toMaybe = runMay
  evalArgs = Just . fmap May

eval :: Program -> Integer
eval = S.eval @May
