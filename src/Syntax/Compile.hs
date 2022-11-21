module Syntax.Compile (compile) where

import Data.Either (lefts, rights)
import qualified Parsing.Parser.Syntax as P (Constant (..), Declaration (..), Expression (..), Operator (..), Program (Program))
import qualified Syntax.Syntax as S (ConstantDefinition (ConstantDefinition), Expression (..), FunctionDefinition (FunctionDefinition), FunctionIdentifier (..), Program (Program), VariableIdentifier (..))

compile :: P.Program -> S.Program
compile (P.Program declarations mainExpression) =
  S.Program
    (fmap compileDefinition $ lefts $ declarations)
    (fmap compileConstant $ rights $ declarations)
    (compileExpression mainExpression)

compileDefinition :: P.Declaration -> S.FunctionDefinition
compileDefinition (P.Declaration id args expr) =
  S.FunctionDefinition
    (S.FunctionIdentifier id)
    (fmap S.VariableIdentifier args)
    (compileExpression expr)

compileExpression :: P.Expression -> S.Expression
compileExpression (P.IntegerLiteral n) = S.Literal n
compileExpression (P.Variable id) = S.Variable $ S.VariableIdentifier id
compileExpression (P.Application id args) =
  S.Application
    (S.FunctionIdentifier id)
    (fmap compileExpression args)
compileExpression (P.BinaryOperator P.Addition l r) =
  S.Addition
    (compileExpression l)
    (compileExpression r)
compileExpression (P.BinaryOperator P.Subtraction l r) =
  S.Addition
    (compileExpression l)
    (S.Multiplication (S.Literal (-1)) (compileExpression r))
compileExpression (P.BinaryOperator P.Multiplication l r) =
  S.Multiplication
    (compileExpression l)
    (compileExpression r)
compileExpression (P.Negation ex) =
  S.Multiplication
    (S.Literal (-1))
    (compileExpression ex)
compileExpression (P.Conditional guard thenBranch elseBranch) =
  S.Conditional
    (compileExpression guard)
    (compileExpression thenBranch)
    (compileExpression elseBranch)

compileConstant :: P.Constant -> S.ConstantDefinition
compileConstant (P.Constant id val) = S.ConstantDefinition (S.VariableIdentifier id) val

-- >>> compileExpression (P.BinaryOperator P.Subtraction (P.Negation (P.IntegerLiteral 69)) (P.IntegerLiteral 10))
-- Addition (Multiplication (Literal (-1)) (Literal 69)) (Multiplication (Literal (-1)) (Literal 10))

-- >>> compile (P.Program [P.Declaration "s" ["x"] (P.Conditional (P.Variable "x") (P.IntegerLiteral 0) (P.Application "f" [P.Variable "x",P.BinaryOperator P.Subtraction (P.IntegerLiteral 0) (P.Variable "x")])),P.Declaration "f" ["x","y"] (P.Conditional (P.Variable "x") (P.IntegerLiteral 1) (P.Conditional (P.Variable "y") (P.Negation (P.IntegerLiteral 1)) (P.Application "f" [P.BinaryOperator P.Subtraction (P.Variable "x") (P.Variable "l"),P.BinaryOperator P.Subtraction (P.Variable "y") (P.IntegerLiteral 1)])))] (P.BinaryOperator P.Addition (P.IntegerLiteral 1) (P.IntegerLiteral 2)))
-- Program [FunctionDefinition (FunctionIdentifier "s") [VariableIdentifier "x"] (Conditional (Variable (VariableIdentifier "x")) (Literal 0) (Application (FunctionIdentifier "f") [Variable (VariableIdentifier "x"),Addition (Literal 0) (Multiplication (Literal (-1)) (Variable (VariableIdentifier "x")))])),FunctionDefinition (FunctionIdentifier "f") [VariableIdentifier "x",VariableIdentifier "y"] (Conditional (Variable (VariableIdentifier "x")) (Literal 1) (Conditional (Variable (VariableIdentifier "y")) (Multiplication (Literal (-1)) (Literal 1)) (Application (FunctionIdentifier "f") [Addition (Variable (VariableIdentifier "x")) (Multiplication (Literal (-1)) (Variable (VariableIdentifier "l"))),Addition (Variable (VariableIdentifier "y")) (Multiplication (Literal (-1)) (Literal 1))])))] (Addition (Literal 1) (Literal 2))
