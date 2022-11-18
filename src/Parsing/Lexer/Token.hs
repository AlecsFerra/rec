module Parsing.Lexer.Token (Token (..)) where

data Token
  = Literal Integer
  | Name String
  | Plus
  | Dash
  | Star
  | If
  | Then
  | Else
  | OpenParen
  | CloseParen
  | Comma
  | Equal
  deriving (Show, Eq)
