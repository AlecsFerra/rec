{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Semantics.Environment (Environment (lookup), withDefault, insert, map) where

import Prelude hiding (lookup)

newtype Environment k v = MkEnv
  { lookup :: k -> v
  }
  deriving (Functor)

withDefault :: v -> Environment k v
withDefault = MkEnv . const

insert :: forall k v. Eq k => Environment k v -> k -> v -> Environment k v
insert env k v = MkEnv lookup'
  where
    lookup' :: k -> v
    lookup' k' | k == k' = v
    lookup' k' = lookup env k'
