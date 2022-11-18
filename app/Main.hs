{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Main (main) where

import Control.Exception (IOException, catch)
import Data.Maybe (listToMaybe)
import Parsing.Parser (ParseError (..), parse)
import Semantics.Check (CheckError (..), checkProgram)
import qualified Semantics.Lazy.Semantics as L (eval)
import qualified Semantics.Strict.Semantics as S (eval)
import Syntax.Compile (compile)
import Syntax.Syntax (FunctionIdentifier (..), VariableIdentifier (VariableIdentifier))
import System.Environment (getArgs, getProgName)
import System.Exit (exitFailure)
import Text.Printf (printf)
import Prelude hiding (error, fail)

error :: Bool -> String -> IO ()
error True msg = fail msg
error False _ = pure ()

fail :: String -> IO a
fail msg = putStrLn msg >> exitFailure

prettyParseError :: String -> ParseError -> String
prettyParseError fileName error = printf "Error while parsing '%s': %s" fileName $ pretty error
  where
    pretty (UnexpectedCharacter c) = printf "Unexpected '%c'" c
    pretty (UnexpectedToken t) = printf "Unexpected '%s'" t
    pretty UnexpectedEOF = "Unexpected end of file"

prettyCheckError :: String -> CheckError -> String
prettyCheckError fileName error = printf "Error while checking '%s': %s" fileName (pretty error :: String)
  where
    pretty (DuplicateFunctionIdentifier (FunctionIdentifier id)) = printf "function %s is declared multiple times" id
    pretty (UnknownFunction (FunctionIdentifier id)) = printf "unknown function %s" id
    pretty (WrongArity (FunctionIdentifier id) expected got) = printf "function %s expected %d arguments but got %d" id expected got
    pretty (UnknownVariable (VariableIdentifier id)) = printf "unknown variable %s" id
    pretty (DuplicateParameter (VariableIdentifier id)) = printf "parameter %s is declared multiple times in the same function definition" id

data EvaluationMode = Lazy | Strict

parseMode :: String -> IO EvaluationMode
parseMode "-s" = pure Strict
parseMode "--strict" = pure Strict
parseMode "-l" = pure Lazy
parseMode "--lazy" = pure Lazy
parseMode m = fail $ printf "Unrecognized execution flag '%s'" m

main :: IO ()
main = do
  programName <- getProgName
  args <- getArgs
  error (length args `notElem` [1, 2]) $ printf "Usage: %s source.rec [-l|-s|--strict|--lazy]" programName

  let sourceName = head args
  source <- readFile sourceName `catch` const @_ @IOException (fail $ printf "Source file '%s' not found" sourceName)

  parsed <- either (fail . prettyParseError sourceName) pure $ parse source

  let compiled = compile parsed
  either (fail . prettyCheckError sourceName) pure $ checkProgram compiled

  mode <- maybe (pure Strict) parseMode $ listToMaybe $ tail args
  case mode of
    Lazy -> print $ L.eval compiled
    Strict -> print $ S.eval compiled
