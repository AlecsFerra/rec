{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Parsing.Parser.Parser (parse, ParsingError (..)) where

import Control.Applicative (Alternative (..))
import Control.Monad (void)
import Data.Foldable (Foldable (..))
import GHC.Base (Applicative (..))
import Parsing.Lexer.Token (Token (..))
import Parsing.Parser.Syntax (Constant (Constant), Declaration (Declaration), Expression (..), Operator (..), Program (..))
import Parsing.ParserCombinator (FromStream (..), pluck, satisfies)
import qualified Parsing.ParserCombinator as P (Parser (..))

data ParsingError
  = Unexpected Token
  | UnexpectedEOF
  deriving (Show)

type Parser a = P.Parser [Token] ParsingError a

instance FromStream [Token] ParsingError where
  fromStream [] = UnexpectedEOF
  fromStream (t : _) = Unexpected t

is :: Token -> Parser ()
is = void . satisfies . (==)

parenthesize :: Parser a -> Parser a
parenthesize a = (is OpenParen *> a <* is CloseParen) <|> a

if_ :: Parser Expression
if_ =
  Conditional
    <$> (is If *> expression)
    <*> (is Then *> expression)
    <*> (is Else *> expression)

assocL :: Parser (a -> a -> a) -> Parser a -> Parser a
assocL sep a = liftA2 squash a (many (liftA2 (,) sep a))
  where
    squash = foldl' (\acc (combine, a) -> combine acc a)

additionSubtraction :: Parser Expression
additionSubtraction = assocL parse multiplication
  where
    parse =
      BinaryOperator Addition <$ is Plus
        <|> BinaryOperator Subtraction <$ is Dash

multiplication :: Parser Expression
multiplication = assocL (BinaryOperator Multiplication <$ is Star) negation

negation :: Parser Expression
negation = neg <|> pos <|> application
  where
    neg = Negation <$> (is Dash *> application)
    pos = is Plus *> application

name :: Parser String
name = pluck $ \case
  (Name s) -> Just s
  _ -> Nothing

separated :: Token -> Parser a -> Parser [a]
separated sep a = too <|> none
  where
    none = pure []
    too = liftA2 (:) a (many (is sep *> a))

application :: Parser Expression
application = app <|> variable
  where
    app = Application <$> name <*> (is OpenParen *> args <* is CloseParen)
    args = separated Comma expression

variable :: Parser Expression
variable = (Variable <$> name) <|> literal

literal :: Parser Expression
literal = fmap IntegerLiteral integer

expression :: Parser Expression
expression = parenthesize (if_ <|> additionSubtraction)

-- Checking some expressions
-- >>> P.runParser expression []
-- Left UnexpectedEOF
-- >>> P.runParser expression [Plus, Literal 1]
-- Right (IntegerLiteral 1,[])
-- >>> P.runParser expression [Dash, Literal 1, Dash, Literal  1]
-- Right (BinaryOperator Subtraction (Negation (IntegerLiteral 1)) (IntegerLiteral 1),[])

-- Try the factorial program
-- >>> P.runParser expression [If,Name "x",Then,Literal 1,Else,Name "x",Star,Name "fact",OpenParen,Name "x",Dash,Literal 1,CloseParen]
-- Right (Conditional (Variable "x") (IntegerLiteral 1) (BinaryOperator Addition (Variable "x") (Application "fact" [BinaryOperator Subtraction (Variable "x") (IntegerLiteral 1)])),[])

-- Associativity
-- >>> P.runParser expression [Plus, Literal 1, Plus, Literal 2, Plus, Literal 3]
-- Right (BinaryOperator Addition (BinaryOperator Addition (IntegerLiteral 1) (IntegerLiteral 2)) (IntegerLiteral 3),[])

-- Order
-- >>> P.runParser expression [Plus, Literal 1, Plus, Literal 2, Star, Literal 3]
-- Right (BinaryOperator Addition (IntegerLiteral 1) (BinaryOperator Multiplication (IntegerLiteral 2) (IntegerLiteral 3)),[])
-- >>> P.runParser expression [Plus, Literal 1, Star, Literal 2, Plus, Literal 3]
-- Right (BinaryOperator Addition (BinaryOperator Multiplication (IntegerLiteral 1) (IntegerLiteral 2)) (IntegerLiteral 3),[])

declaration :: Parser Declaration
declaration = Declaration <$> name <*> args <* is Equal <*> expression
  where
    args = is OpenParen *> separated Comma name <* is CloseParen

integer :: Parser Integer
integer = pluck $ \case
  (Literal s) -> Just s
  _ -> Nothing

constant :: Parser Constant
constant = Constant <$> name <* is Equal <*> integer

program :: Parser Program
program = Program <$> many (fmap Left declaration <|> fmap Right constant) <*> expression

parse :: [Token] -> Either ParsingError Program
parse t = case P.runParser program t of
  Left err -> Left err
  Right (tokens, []) -> Right tokens
  Right (_, rest) -> Left $ fromStream rest

-- Parse the factorial program
-- >>> parse [Name "fact",OpenParen,Name "x",CloseParen,Equal, Literal 11]
-- Right [Declaration "fact" ["x"] (IntegerLiteral 11)]
-- >>> parse [Name "fact",OpenParen,Name "x",CloseParen,Equal,If,Name "x",Then,Literal 1,Else,Name "x",Star,Name "fact",OpenParen,Name "x",Dash,Literal 1,CloseParen]
-- Right [Declaration "fact" ["x"] (Conditional (Variable "x") (IntegerLiteral 1) (BinaryOperator Multiplication (Variable "x") (Application "fact" [BinaryOperator Subtraction (Variable "x") (IntegerLiteral 1)])))]
