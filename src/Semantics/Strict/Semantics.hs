module Semantics.Strict.Semantics (eval) where

import Control.Applicative (Applicative (liftA2))
import Control.Monad (forM)
import Data.Bool (bool)
import Data.Foldable (Foldable (foldl'))
import Semantics.Environment (Environment (lookup), insert, withDefault)
import Semantics.FixPoint (fix)
import Syntax.Syntax (Expression (..), FunctionDefinition (..), FunctionIdentifier (..), Program (..), VariableIdentifier (..))
import Util ((.:.))
import Prelude hiding (lookup)

type FSemantic = Env -> Maybe Integer

data Functional = Functional [VariableIdentifier] Expression FSemantic

type FEnv = Environment FunctionIdentifier Functional

type Env = Environment VariableIdentifier Integer

makeEnv :: Env -> [VariableIdentifier] -> [Integer] -> Env
makeEnv base = foldl' (uncurry . insert) base .:. zip

semantics :: Expression -> FEnv -> Env -> Maybe Integer
semantics e fenv env = run e
  where
    semantics' e = semantics e fenv env

    run (Variable id) = Just $ lookup env id
    run (Literal n) = Just n
    run (Addition l r) = liftA2 (+) (semantics' l) (semantics' r)
    run (Multiplication l r) = liftA2 (*) (semantics' l) (semantics' r)
    run (Conditional guard thenBranch elseBranch) =
      semantics' guard >>= (bool (semantics' thenBranch) (semantics' elseBranch) . (/= 0))
    run (Application fi args) = do
      args <- forM args semantics'
      let (Functional argNames _ functional) = lookup fenv fi
      let env' = makeEnv env argNames args
      functional env'

makeBottom :: [FunctionDefinition] -> FEnv
makeBottom = foldl' (uncurry . insert) defaultFenv . fmap makeEmpty
  where
    makeEmpty (FunctionDefinition id args body) = (id, Functional args body (const Nothing))
    defaultFenv = withDefault $ error "Precondition violated the program called an undefined function"

step :: FEnv -> FEnv
step fenv = fmap step fenv
  where
    step (Functional args body _) = Functional args body (semantics body fenv)

eval :: Program -> Integer
eval (Program definitions mainExpression) = fix (makeBottom definitions) step eval'
  where
    eval' f = semantics mainExpression f $ withDefault $ error "Precondition violated the program called an undefined variable"
