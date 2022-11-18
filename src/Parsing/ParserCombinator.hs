{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Parsing.ParserCombinator (Parser, FromStream (..), satisfies, pluck, runParser) where

import Control.Applicative (Alternative (empty, (<|>)))
import Control.Arrow (Arrow (first))

-- | A parser combinator.
--  It parses elements of type @p@ while reading a stream of
--  type @s@, it can return @e@ as an error.
newtype Parser s e p = Parser
  { -- | The parsing function
    runParser :: s -> Either e (p, s)
  }

-- | An error @e@ that can be constructed from a stream @s@.
class FromStream s e where
  fromStream :: s -> e

instance Functor (Parser s e) where
  fmap f = Parser . fmap (fmap $ first f) . runParser

instance Applicative (Parser s e) where
  pure p = Parser $ \s -> Right (p, s)

  (Parser atob) <*> (Parser a) =
    Parser $ \s -> do
      (atob, s) <- atob s
      (a, s) <- a s
      pure (atob a, s)

instance (FromStream s e) => Alternative (Parser s e) where
  empty = Parser $ Left . fromStream

  (Parser a) <|> (Parser b) =
    Parser $ \s -> case (a s, b s) of
      (Left _, b) -> b
      (a, _) -> a

-- | Parse the first element of a list of *c* only if it satisfies a predicate
satisfies :: FromStream [c] e => (c -> Bool) -> Parser [c] e c
satisfies p = Parser $ \case
  c : cs | p c -> Right (c, cs)
  cs -> Left $ fromStream cs

pluck :: FromStream [c] e => (c -> Maybe a) -> Parser [c] e a
pluck f =
  Parser $ \case
    t : ts -> case f t of
      Just res -> Right (res, ts)
      Nothing -> Left $ fromStream $ t : ts
    ts -> Left $ fromStream ts
