{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Semantics.Semantics (eval, EvalStrategy (..)) where

import Data.Bool (bool)
import Data.Foldable (Foldable (..))
import GHC.Base (Applicative (..))
import Semantics.Environment (Environment, insert, lookup, withDefault)
import Semantics.FixPoint (fix)
import Syntax.Syntax (Expression (..), FunctionDefinition (..), FunctionIdentifier (..), Program (..), VariableIdentifier (..))
import Util ((.:.))
import Prelude hiding (lookup)

type FSemantic m = Env m -> Maybe Integer

data Function m = Function [VariableIdentifier] Expression (FSemantic m)

type FEnv m = Environment FunctionIdentifier (Function m)

type Env m = Environment VariableIdentifier (m Integer)

data EvalStrategy m = EvalStrategy
  { maybeM :: forall a. m a -> Maybe a,
    arguments :: [Maybe Integer] -> Maybe [m Integer]
  }

eval :: forall m. EvalStrategy m -> Program -> Integer
eval strategy (Program defs main) = fix (makeBottom defs) step (\fenv -> semantics main fenv baseEnv)
  where
    baseEnv :: Environment k v
    baseEnv =
      withDefault $ error "Precondition violated the program referenced an undefined identifier"

    step :: FEnv m -> FEnv m
    step fenv = fmap step' fenv
      where
        step' (Function args body _) = Function args body (semantics body fenv)

    makeBottom :: [FunctionDefinition] -> FEnv m
    makeBottom = foldl' (uncurry . insert) baseEnv . fmap makeEmpty
      where
        makeEmpty (FunctionDefinition id args body) = (id, Function args body (const Nothing))

    makeEnv :: Env m -> [VariableIdentifier] -> [m Integer] -> Env m
    makeEnv base = foldl' (uncurry . insert) base .:. zip

    semantics :: Expression -> FEnv m -> Env m -> Maybe Integer
    semantics e fenv env = semantics' e
      where
        semantics' (Variable id) = maybeM strategy $ lookup env id
        semantics' (Literal n) = Just n
        semantics' (Addition l r) = liftA2 (+) (semantics' l) (semantics' r)
        semantics' (Multiplication l r) = liftA2 (*) (semantics' l) (semantics' r)
        semantics' (Conditional guard thenClause elseClause) =
          semantics' guard >>= (bool (semantics' thenClause) (semantics' elseClause) . (/= 0))
        semantics' (Application id args) = do
          args <- arguments strategy $ fmap semantics' args
          let (Function argNames _ functional) = lookup fenv id
          let env' = makeEnv baseEnv argNames args
          functional env'