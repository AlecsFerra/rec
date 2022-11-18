module Semantics.Check (checkProgram, CheckError (..)) where

import Control.Monad (foldM)
import Data.Foldable (forM_)
import Semantics.Environment (Environment, insert, lookup, withDefault)
import Syntax.Syntax (Expression (..), FunctionDefinition (FunctionDefinition), FunctionIdentifier (..), Program (Program), VariableIdentifier (..))
import Util (guard', note)
import Prelude hiding (lookup)

type Arity = Int

type FEnv = Environment FunctionIdentifier (Maybe Arity)

type Env = Environment VariableIdentifier Bool

data CheckError
  = DuplicateFunctionIdentifier FunctionIdentifier
  | UnknownFunction FunctionIdentifier
  | WrongArity FunctionIdentifier Arity Arity
  | UnknownVariable VariableIdentifier
  | DuplicateParameter VariableIdentifier
  deriving (Show)

checkProgram :: Program -> Either CheckError ()
checkProgram (Program definitions main) = do
  fenv <- foldM buildFenv (withDefault Nothing) definitions
  forM_ definitions (checkFunction fenv)
  checkExpression fenv (withDefault False) main

checkFunction :: FEnv -> FunctionDefinition -> Either CheckError ()
checkFunction fenv (FunctionDefinition _ args body) = do
  env <- checkArgs args
  checkExpression fenv env body
  where
    checkArgs = foldM comb (withDefault False)
    comb env id | lookup env id = Left $ DuplicateParameter id
    comb env id = Right $ insert env id True

buildFenv :: FEnv -> FunctionDefinition -> Either CheckError FEnv
buildFenv fenv (FunctionDefinition id args _) = do
  unusedName fenv id
  pure $ insert fenv id $ Just $ length args
  where
    unusedName fenv id = case lookup fenv id of
      Nothing -> pure ()
      Just _ -> Left $ DuplicateFunctionIdentifier id

checkExpression :: FEnv -> Env -> Expression -> Either CheckError ()
checkExpression fenv env = checkExpression'
  where
    checkExpression' (Variable id) | lookup env id = pure ()
    checkExpression' (Variable id) = Left $ UnknownVariable id
    checkExpression' (Literal _) = pure ()
    checkExpression' (Conditional guard thenBranch elseBranch) =
      checkExpression' guard
        *> checkExpression' thenBranch
        *> checkExpression' elseBranch
    checkExpression' (Addition l r) = checkExpression' l *> checkExpression' r
    checkExpression' (Multiplication l r) = checkExpression' l *> checkExpression' r
    checkExpression' (Application id args) = do
      forM_ args checkExpression'
      arity <- note (UnknownFunction id) $ lookup fenv id
      guard' (arity == length args) $ WrongArity id arity $ length args

-- >>> checkProgram (Program [FunctionDefinition (FunctionIdentifier "s") [VariableIdentifier "x"] (Conditional (Variable (VariableIdentifier "x")) (Literal 0) (Application (FunctionIdentifier "f") [Variable (VariableIdentifier "x"),Addition (Literal 0) (Multiplication (Literal (-1)) (Variable (VariableIdentifier "x")))])),FunctionDefinition (FunctionIdentifier "f") [VariableIdentifier "x",VariableIdentifier "y"] (Conditional (Variable (VariableIdentifier "x")) (Literal 1) (Conditional (Variable (VariableIdentifier "y")) (Multiplication (Literal (-1)) (Literal 1)) (Application (FunctionIdentifier "f") [Addition (Variable (VariableIdentifier "x")) (Multiplication (Literal (-1)) (Variable (VariableIdentifier "l"))),Addition (Variable (VariableIdentifier "y")) (Multiplication (Literal (-1)) (Literal 1))])))] (Addition (Literal 1) (Literal 2)))
-- Left (UnknownVariable (VariableIdentifier "l"))
