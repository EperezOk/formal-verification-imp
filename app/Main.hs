{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Monad ((<=<))

import Data.Map (Map)
import Data.Map qualified as Map

import Z3.Monad qualified as Z3

import System.Console.ANSI (setSGR, SGR(..), Color(..), ConsoleLayer(..), ColorIntensity(..))

import Interpreter (evalStmt, Id)
import Parser (parseSrc)
import SMT qualified

main :: IO ()
main = runZ3WithInlineSpec "examples/inline-spec.imp"


-- | Interpreter

runInterpreter :: FilePath -> IO ()
-- parses a file and interprets the program
runInterpreter path = do
  program <- readFile path
  case parseSrc path program of
    Left err  -> print err
    Right stmt -> do
      print stmt -- print AST
      case evalStmt [] stmt of
        Nothing  -> putStrLn "Scope error"
        Just res -> print res -- print final scope

runInterpreterInteractive :: String -> IO ()
-- parses a string and interprets the program
runInterpreterInteractive input = case parseSrc "<interactive>" input of
  Left err  -> print err
  Right cmd -> do
    print cmd               -- print AST
    print $ evalStmt [] cmd -- print final scope


-- | Solver Configuration

z3Logic :: Z3.Logic
-- closed quantifier-free formulas over the theory of fixed-size bitvectors.
-- reference: https://smt-lib.org/logics.shtml
z3Logic = Z3.QF_BV

z3Opts :: Z3.Opts
z3Opts = Z3.opt "MODEL" True -- enable model generation

solve :: Z3.Z3 a -> IO a
solve = Z3.evalZ3With (Just z3Logic) z3Opts


-- | Run against inlined constraints

data VerificationResult = Success | Undef | Failure String

runZ3WithInlineSpec :: FilePath -> IO ()
runZ3WithInlineSpec path = do
  program <- readFile path
  case parseSrc path program of
    Left err  -> print err
    Right ast -> do
      result <- solve $ do
        SMT.buildZ3Computation Map.empty Map.empty ast
        Z3.solverCheckAndGetModel >>= \case
          (Z3.Sat, Just model) -> do
            modelStr <- Z3.modelToString model
            return $ Failure ("Counterexample:\n" ++ modelStr)
          (Z3.Sat, Nothing) -> return $ Failure "Could not get the counterexample model"
          (Z3.Unsat, _) -> return Success
          (Z3.Undef, _) -> return Undef
      case result of
        Success -> do
          setSGR [SetColor Foreground Vivid Green]
          putStrLn "SUCCESS: All assertions hold"
          setSGR [Reset]
        Undef -> do
          setSGR [SetColor Foreground Vivid Yellow]
          putStrLn "WARNING: Undefined result. Please run again with a different configuration"
          setSGR [Reset]
        Failure modelStr -> do
          setSGR [SetColor Foreground Vivid Red]
          putStrLn "ERROR: Assertions do not hold.\n"
          putStrLn modelStr
          setSGR [Reset]


-- | Run with input and output constraints

runZ3OverflowExample :: FilePath -> IO ()
runZ3OverflowExample = runZ3WithConstraints inConstrs outConstrs
  where inConstrs  = Map.fromList [("deposit", SMT.ConstrainGte 0)]
        outConstrs = Map.fromList [("balance", SMT.ConstrainLte (-1))]

runZ3FinalBalanceValueExample :: FilePath -> IO ()
runZ3FinalBalanceValueExample = runZ3WithConstraints inConstrs outConstrs
  where inConstrs  = Map.fromList [("deposit", SMT.ConstrainGte 0)]
        outConstrs = Map.fromList [("balance", SMT.ConstrainEq 1000)]

runZ3FinalBalanceValueExample' :: FilePath -> IO ()
runZ3FinalBalanceValueExample' = runZ3WithConstraints inConstrs outConstrs
  where inConstrs  = Map.fromList [("deposit", SMT.ConstrainGte 0)]
        outConstrs = Map.fromList [("balance", SMT.ConstrainEq 50000)]

runZ3GcdExample :: FilePath -> IO ()
runZ3GcdExample = runZ3WithConstraints inConstrs outConstrs
  where inConstrs  = Map.empty
        outConstrs = Map.fromList [ ("a", SMT.ConstrainEq 135),
                                    ("b", SMT.ConstrainEq 135),
                                    ("d", SMT.ConstrainEq 0) ]

runZ3WithConstraints :: Map Id SMT.Constraint -> Map Id SMT.Constraint -> FilePath -> IO ()
runZ3WithConstraints inConstrs outConstrs path = do
  program <- readFile path
  case parseSrc path program of
    Left err  -> print err
    Right ast -> putStrLn <=< solve $ do
      SMT.buildZ3Computation inConstrs outConstrs ast
      Z3.solverCheckAndGetModel >>= \case
        (Z3.Sat, Just model) -> Z3.modelToString model -- show variable assignments
        (result, _)          -> return (show result)   -- show `Unsat` or `Undef`
