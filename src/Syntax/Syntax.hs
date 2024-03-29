module Syntax.Syntax (Expression (..), FunctionDefinition (..), Program (..), FunctionIdentifier (..), VariableIdentifier (..), ConstantDefinition (..)) where

newtype VariableIdentifier = VariableIdentifier String
  deriving (Eq, Show, Ord)

newtype FunctionIdentifier = FunctionIdentifier String
  deriving (Eq, Show, Ord)

data Expression
  = Literal Integer
  | Variable VariableIdentifier
  | Addition Expression Expression
  | Multiplication Expression Expression
  | Conditional Expression Expression Expression
  | Application FunctionIdentifier [Expression]
  deriving (Show)

data FunctionDefinition = FunctionDefinition FunctionIdentifier [VariableIdentifier] Expression
  deriving (Show)

data ConstantDefinition = ConstantDefinition VariableIdentifier Integer
  deriving (Show)

data Program = Program [FunctionDefinition] [ConstantDefinition] Expression
  deriving (Show)
