{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Monad ((<=<))

import Data.Map (Map)
import Data.Map qualified as Map

import Z3.Monad qualified as Z3

import Interpreter (evalStmt, Id)
import Parser (parseSrc)
import SMT qualified

main :: IO ()
main = runZ3OverflowExample "examples/overflow-z3.imp"

-- | Interpreter

runInterpreter :: FilePath -> IO ()
runInterpreter path = do
  program <- readFile path
  case parseSrc path program of
    Left err  -> print err
    Right stmt -> do
      print stmt -- print AST
      case evalStmt [] stmt of
        Nothing  -> putStrLn "Scope error"
        Just res -> print res -- print final scope

-- | Solver

z3Logic :: Z3.Logic
-- closed quantifier-free formulas over the theory of fixed-size bitvectors.
-- reference: https://smt-lib.org/logics.shtml
z3Logic = Z3.QF_BV

z3Opts :: Z3.Opts
z3Opts = Z3.opt "MODEL" True -- enable model generation

runZ3OverflowExample :: FilePath -> IO ()
runZ3OverflowExample = runZ3 inConstrs outConstrs
  where inConstrs  = Map.fromList [("deposit", SMT.ConstrainGte 0)]
        outConstrs = Map.fromList [("balance", SMT.ConstrainLte (-1))]

runZ3FinalBalanceValueExample :: FilePath -> IO ()
runZ3FinalBalanceValueExample = runZ3 inConstrs outConstrs
  where inConstrs  = Map.fromList [("deposit", SMT.ConstrainGte 0)]
        outConstrs = Map.fromList [("balance", SMT.ConstrainEq 1000)]

runZ3FinalBalanceValueExample' :: FilePath -> IO ()
runZ3FinalBalanceValueExample' = runZ3 inConstrs outConstrs
  where inConstrs  = Map.fromList [("deposit", SMT.ConstrainGte 0)]
        outConstrs = Map.fromList [("balance", SMT.ConstrainEq 50000)]

runZ3GcdExample :: FilePath -> IO ()
runZ3GcdExample = runZ3 inConstrs outConstrs
  where inConstrs  = Map.empty
        outConstrs = Map.fromList [ ("a", SMT.ConstrainEq 135),
                                    ("b", SMT.ConstrainEq 135),
                                    ("d", SMT.ConstrainEq 0) ]

runZ3 :: Map Id SMT.Constraint -> Map Id SMT.Constraint -> FilePath -> IO ()
runZ3 inConstrs outConstrs path = do
  program <- readFile path
  case parseSrc path program of
    Left err  -> print err
    Right ast -> putStrLn <=< solve $ do
      SMT.buildZ3Computation inConstrs outConstrs ast
      Z3.solverCheckAndGetModel >>= \case
        (Z3.Sat, Just model) -> Z3.modelToString model -- show variable assignments
        (result, _)          -> return (show result)   -- show `Unsat` or `Undef`
  where solve = Z3.evalZ3With (Just z3Logic) z3Opts
