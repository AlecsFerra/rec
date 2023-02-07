{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Generalized evaluation for a rec program
module Semantics.Semantics (eval, EvalStrategy (..)) where

import Control.Arrow (Arrow (second))
import Data.Bool (bool)
import Data.Foldable (Foldable (..))
import Data.Maybe (mapMaybe)
import GHC.Base (Applicative (..))
import Semantics.Environment (Environment, insert, lookup, withDefault)
import Syntax.Syntax (ConstantDefinition (..), Expression (..), FunctionDefinition (..), FunctionIdentifier (..), Program (..), VariableIdentifier (..))
import Util ((.:.))
import Prelude hiding (lookup)

-- | The semantic of a function operating of values wrapped in m
type FSemantic m = Env m -> Maybe Integer

-- | The whole function information
data Function m = Function
  -- | The identifiers of the arguments
  [VariableIdentifier]
  -- | The expression that defines his semantics
  Expression
  -- | The semantics of the function
  (FSemantic m)

-- | The function environment
type FEnv m = Environment FunctionIdentifier (Function m)

-- | The variable environment
type Env m = Environment VariableIdentifier (m Integer)

-- | An empty generic base environment that throws an error when any identifier is
-- looked-up
-- >>> lookup baseEnv "any"
-- *** Exception: Precondition violated the program referenced an undefined identifier
baseEnv :: Environment k v
baseEnv =
  withDefault $
    error
      "Precondition violated the program referenced an undefined identifier"

-- | Apply to a function the lfp on the cpo starting from the bottom
-- >>> applyFix (\a -> if a == 11 then Just a else Nothing) 0 (+ 1)
-- 11
applyFix ::
  -- | The function on witch the lfp gets applied
  (a -> Maybe b) ->
  -- | The bottom element
  a ->
  -- | A continuos function on the cpo *a*
  (a -> a) ->
  -- | The result of applying the lfp to the function
  b
applyFix g bottom f = head $ mapMaybe g sequence
  where
    -- Construct the function application sequence
    -- f^0 bottom, f^1 bottom, f^2 bottom, ...
    sequence = iterate f bottom

-- | The evaluation strategy when working on an environment with values
-- wrapped by *m*
data EvalStrategy m = EvalStrategy
  { -- | Partial function unwrapping a value
    toMaybe :: forall a. m a -> Maybe a,
    -- | Partial function describing how should arguments must be evaluated
    evalArgs :: [Maybe Integer] -> Maybe [m Integer],
    -- | Lift an unwrapped value in m
    pure :: forall a. a -> m a
  }

-- | Eval a program using the strategy *m*, the program must have been already checked for
-- invalid references to variables and/or identifiers
eval :: forall m. EvalStrategy m -> Program -> Integer
eval (EvalStrategy toMaybe evalArgs pure) (Program defs consts main) =
  -- Find the fix point of F starting from the bottom environment
  applyFix (\fenv -> semantics main fenv globalEnv) (makeBottom defs) step
  where
    -- | The global variable environment witch is filled with the constants
    -- defined in the program
    globalEnv :: Env m
    globalEnv = uncurry (makeEnv baseEnv) $ unzip $ fmap (second pure . toTuple) consts
      where
        toTuple (ConstantDefinition a b) = (a, b)

    -- | A continuos function on *Fenv m* witch fixpoint is the solution of the equation
    -- induced by the program (defined as the functional F described in 9.3 pag. 146)
    step :: FEnv m -> FEnv m
    step fenv = fmap step' fenv
      where
        step' (Function args body _) = Function args body $ semantics body fenv

    -- | Construct the bottom function environment where every function is always undefined
    makeBottom :: [FunctionDefinition] -> FEnv m
    makeBottom = foldl' (uncurry . insert) baseEnv . fmap makeEmpty
      where
        makeEmpty (FunctionDefinition id args body) = (id, Function args body $ const Nothing)

    -- | Add to a base environment
    makeEnv ::
      -- | The base environment
      Env m ->
      -- | The identifiers that should be added
      [VariableIdentifier] ->
      -- | Their values the length must be the same as the identifier lists
      [m Integer] ->
      Env m
    makeEnv base = foldl' (uncurry . insert) base .:. zip

    -- | Semantic function of an expression as defined in 9.3 pag. 144
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
          -- Apply the argument evaluation strategy
          args <- evalArgs $ fmap semantics' args
          let (Function argNames _ functional) = lookup fenv id
          let env' = makeEnv globalEnv argNames args
          functional env'
