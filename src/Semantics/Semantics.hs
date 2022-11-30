{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Semantics.Semantics (eval, EvalStrategy (..)) where

import Control.Arrow (Arrow (second))
import Data.Bool (bool)
import Data.Foldable (Foldable (..))
import GHC.Base (Applicative (..))
import Semantics.Environment (Environment, insert, lookup, withDefault)
import Semantics.FixPoint (applyFixpoint)
import Syntax.Syntax (ConstantDefinition (..), Expression (..), FunctionDefinition (..), FunctionIdentifier (..), Program (..), VariableIdentifier (..))
import Util ((.:.))
import Prelude hiding (lookup)

type FSemantic m = Env m -> Maybe Integer

data Function m = Function [VariableIdentifier] Expression (FSemantic m)

type FEnv m = Environment FunctionIdentifier (Function m)

type Env m = Environment VariableIdentifier (m Integer)

data EvalStrategy m = EvalStrategy
  { toMaybe :: forall a. m a -> Maybe a,
    evalArgs :: [Maybe Integer] -> Maybe [m Integer],
    pure :: forall a. a -> m a
  }

-- Eval a program using the strategy m, the program must have been already checked
eval :: forall m. EvalStrategy m -> Program -> Integer
eval (EvalStrategy toMaybe evalArgs pure) (Program defs consts main) =
  -- Find the fix point of F starting from the bottom environment
  applyFixpoint (makeBottom defs) step $ \fenv -> semantics main fenv globalEnv
  where
    -- Base empty environment
    baseEnv :: Environment k v
    baseEnv =
      withDefault $ error "Precondition violated the program referenced an undefined identifier"

    -- build the global environment containing all the constants
    globalEnv :: Env m
    globalEnv = uncurry (makeEnv baseEnv) $ unzip $ fmap (second pure . toTuple) consts
      where
        toTuple (ConstantDefinition a b) = (a, b)

    -- The F defined as described in 9.3 pag. 146
    step :: FEnv m -> FEnv m
    step fenv = fmap step' fenv
      where
        step' (Function args body _) = Function args body $ semantics body fenv

    -- Create the bottom FEnv by their definitions
    makeBottom :: [FunctionDefinition] -> FEnv m
    makeBottom = foldl' (uncurry . insert) baseEnv . fmap makeEmpty
      where
        makeEmpty (FunctionDefinition id args body) = (id, Function args body $ const Nothing)

    -- Add the variables in the environment
    makeEnv :: Env m -> [VariableIdentifier] -> [m Integer] -> Env m
    makeEnv base = foldl' (uncurry . insert) base .:. zip

    -- Semantic function of a term as defined in 9.3 pag. 144
    semantics :: Expression -> FEnv m -> Env m -> Maybe Integer
    semantics e fenv env = semantics' e
      where
        semantics' (Variable id) = toMaybe $ lookup env id
        semantics' (Literal n) = Just n
        semantics' (Addition l r) = liftA2 (+) (semantics' l) (semantics' r)
        semantics' (Multiplication l r) = liftA2 (*) (semantics' l) (semantics' r)
        semantics' (Conditional guard thenClause elseClause) =
          semantics' guard >>= bool (semantics' thenClause) (semantics' elseClause) . (/= 0)
        semantics' (Application id args) = do
          args <- evalArgs $ fmap semantics' args
          let (Function argNames _ functional) = lookup fenv id
          let env' = makeEnv globalEnv argNames args
          functional env'
