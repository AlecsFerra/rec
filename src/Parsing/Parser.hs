module Parsing.Parser (parse, ParseError (..)) where

import Data.Bifunctor (Bifunctor (first))
import qualified Parsing.Lexer.Lexer as L (LexingError (..), lex)
import qualified Parsing.Parser.Parser as P (ParsingError (..), parse)
import Parsing.Parser.Syntax (Program)

data ParseError
  = UnexpectedCharacter Char
  | UnexpectedToken String
  | UnexpectedEOF
  deriving (Show)

parse :: String -> Either ParseError Program
parse = continue err (first err' . P.parse) . L.lex
  where
    err (L.Unexpected c) = UnexpectedCharacter c
    err L.UnexpectedEOF = UnexpectedEOF
    err' (P.Unexpected t) = UnexpectedToken $ show t
    err' P.UnexpectedEOF = UnexpectedEOF

continue :: (a -> c) -> (b -> Either c d) -> Either a b -> Either c d
continue ac _ (Left a) = Left $ ac a
continue _ bcd (Right b) = bcd b

-- Declarations from the book
-- >>> parse "s(x) = if x then 0 else f(x, 0 - x) f (x, y) = if x then 1 else (if y then -1 else f (x - l, y - 1)) 1 + 2"
-- Right (Program [Declaration "s" ["x"] (Conditional (Variable "x") (IntegerLiteral 0) (Application "f" [Variable "x",BinaryOperator Subtraction (IntegerLiteral 0) (Variable "x")])),Declaration "f" ["x","y"] (Conditional (Variable "x") (IntegerLiteral 1) (Conditional (Variable "y") (Negation (IntegerLiteral 1)) (Application "f" [BinaryOperator Subtraction (Variable "x") (Variable "l"),BinaryOperator Subtraction (Variable "y") (IntegerLiteral 1)])))] (BinaryOperator Addition (IntegerLiteral 1) (IntegerLiteral 2)))
