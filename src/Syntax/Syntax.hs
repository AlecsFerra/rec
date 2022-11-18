module Syntax.Syntax (Expression (..), FunctionDefinition (..), Program (..), FunctionIdentifier (..), VariableIdentifier (..)) where

newtype VariableIdentifier = VariableIdentifier String
  deriving (Eq, Show, Ord)

newtype FunctionIdentifier = FunctionIdentifier String
  deriving (Eq, Show, Ord)

data Expression
  = Variable VariableIdentifier
  | Literal Integer
  | Application FunctionIdentifier [Expression]
  | Conditional Expression Expression Expression
  | Addition Expression Expression
  | Multiplication Expression Expression
  deriving (Show)

data FunctionDefinition = FunctionDefinition FunctionIdentifier [VariableIdentifier] Expression
  deriving (Show)

data Program = Program [FunctionDefinition] Expression
  deriving (Show)
