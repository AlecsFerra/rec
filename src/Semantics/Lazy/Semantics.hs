module Semantics.Lazy.Semantics (eval) where

import Data.Foldable (Foldable (..))
import Semantics.Environment (Environment, insert, lookup, withDefault)
import Syntax.Syntax (Expression (..), FunctionIdentifier (..), VariableIdentifier (..), FunctionDefinition (FunctionDefinition), Program (Program))
import Util ((.:.))
import Prelude hiding (lookup)
import Control.Applicative (Applicative(..))
import Data.Bool (bool)
import Semantics.FixPoint (fix)

type FSemantic = Env -> Maybe Integer

data Function = Function [VariableIdentifier] Expression FSemantic

type FEnv = Environment FunctionIdentifier Function

type Env = Environment VariableIdentifier (Maybe Integer)

baseEnv :: Environment k v
baseEnv =
    withDefault $ error "Precondition violated the program referenced an undefined identifier"

makeEnv :: Env -> [VariableIdentifier] -> [Maybe Integer] -> Env
makeEnv base = foldl' (uncurry . insert) base .:. zip

semantics :: Expression -> FEnv -> Env -> Maybe Integer
semantics e fenv env = run e
  where
    semantics' e = semantics e fenv env

    run (Variable id) = lookup env id
    run (Literal n) = Just n
    run (Addition l r) = liftA2 (+) (semantics' l) (semantics' r)
    run (Multiplication l r) = liftA2 (*) (semantics' l) (semantics' r)
    run (Conditional guard thenBranch elseBranch) =
      semantics' guard >>= (bool (semantics' thenBranch) (semantics' elseBranch) . (/= 0))
    run (Application fi args) = do
      let args' = fmap semantics' args
      let (Function argNames _ functional) = lookup fenv fi
      let env' = makeEnv baseEnv argNames args'
      functional env'

makeBottom :: [FunctionDefinition] -> FEnv
makeBottom = foldl' (uncurry . insert) baseEnv . fmap makeEmpty
  where
    makeEmpty (FunctionDefinition id args body) = (id, Function args body (const Nothing))

step :: FEnv -> FEnv
step fenv = fmap step fenv
  where
    step (Function args body _) = Function args body (semantics body fenv)

eval :: Program -> Integer
eval (Program definitions mainExpression) = fix (makeBottom definitions) step eval'
  where
    eval' f = semantics mainExpression f baseEnv
