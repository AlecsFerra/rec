{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeApplications #-}

module Semantics.Strict.Semantics (eval) where

import Data.Functor.Identity (Identity (Identity, runIdentity), runIdentity)
import Semantics.Semantics (EvalStrategy (..))
import qualified Semantics.Semantics as S (eval)
import Syntax.Syntax (Program)
import Util ((<$$>))

newtype Id a = Id
  { runId :: Identity a
  }
  deriving (Functor, Applicative)

instance EvalStrategy Id where
  toMaybe = Just . runIdentity . runId
  evalArgs = ((Id . Identity) <$$>) . sequence

eval :: Program -> Integer
eval = S.eval @Id
