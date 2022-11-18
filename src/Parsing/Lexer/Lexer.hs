{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Parsing.Lexer.Lexer (lex, LexingError (..)) where

import Control.Applicative (Alternative (some), many, (<|>))
import Control.Monad (void)
import Data.Char (isAlpha, isDigit)
import Parsing.Lexer.Token (Token (..))
import Parsing.ParserCombinator (FromStream (..), Parser (runParser), satisfies)
import Prelude hiding (lex)

data LexingError
  = Unexpected Char
  | UnexpectedEOF
  deriving (Eq, Show)

type Lexer p = Parser String LexingError p

instance FromStream String LexingError where
  fromStream [] = UnexpectedEOF
  fromStream (x : _) = Unexpected x

with :: a -> Lexer b -> Lexer a
with a = fmap (const a)

char :: Char -> Lexer Char
char = satisfies . (==)

string :: String -> Lexer String
string = traverse char

keyword :: Lexer Token
keyword =
  If `with` string "if"
    <|> Then `with` string "then"
    <|> Else `with` string "else"

operator :: Lexer Token
operator =
  Plus `with` string "+"
    <|> Dash `with` string "-"
    <|> Star `with` string "*"
    <|> OpenParen `with` string "("
    <|> CloseParen `with` string ")"
    <|> Comma `with` string ","
    <|> Equal `with` string "="

literal :: Lexer Token
literal = Literal . read <$> some (satisfies isDigit)

name :: Lexer Token
name = Name <$> some (satisfies isAlpha)

token :: Lexer Token
token = keyword <|> literal <|> operator <|> name

whitespace :: Lexer ()
whitespace = void (char ' ' <|> char '\t' <|> char '\n')

-- | Lex all the tokens inside a string or return an error
lex :: String -> Either LexingError [Token]
lex s = case runParser (many (many whitespace *> token) <* many whitespace) s of
  Left err -> Left err
  Right (tokens, "") -> Right tokens
  Right (_, rest) -> Left $ fromStream rest

-- Lexing an operator
-- >>> lex "+"
-- Right [Plus]

-- Lexing a number
-- >>> lex "+42"
-- Right [Plus,Literal 42]
-- >>> lex "11"
-- Right [Literal 11]
-- >>> lex "-33"
-- Right [Minus,Literal 33]

-- Lexing a multitude of tokens
-- >>> lex "+ 1223 () pippo *-"
-- Right [Plus,Literal 1223,OpenParen,CloseParen,Name "pippo",Star,Dash]

-- Lexing can leave whitespaces
--- >>> lex "    +   "
-- Right [Plus]

-- Lexing errors on unexpected characters
-- >>> lex " + ⇔ "
-- Left (Unexpected '\8660')

-- Lexing the factorial program
-- >>> lex "fact(x)=if x then 1 else x * fact(x-1)"
-- Right [Name "fact",OpenParen,Name "x",CloseParen,Equal,If,Name "x",Then,Literal 1,Else,Name "x",Star,Name "fact",OpenParen,Name "x",Dash,Literal 1,CloseParen]
