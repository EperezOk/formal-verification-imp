module SMT where

import Control.Monad (foldM, forM_, join, liftM2)

import Data.Map (Map, (!))
import Data.Map qualified as Map
import Data.Maybe (fromJust)

import Z3.Monad (AST, Z3)
import Z3.Monad qualified as Z3

import Interpreter (Stmt(..), BExp(..), AExp(..), Id)

type Z3Var = AST

-- mapping from variable name to z3 variable
type Scope = Map Id Z3Var

data Constraint
  = ConstrainEq Int
  | ConstrainNeq Int
  | ConstrainLte Int
  | ConstrainGte Int
  deriving (Show)

-- | Configuration

bitSize :: Int
-- size of z3 variables and literals for bitvector SMT theory
bitSize = 32

unrollTimes :: Int
-- how far to unroll loops
unrollTimes = 30

-- | Scope management

mkVar :: Id -> Z3 AST
-- create a new z3 variable
mkVar name = Z3.mkFreshBvVar (show name) bitSize

mkScope :: [Id] -> Z3 Scope
-- create a new scope with fresh z3 variables
mkScope names = foldM putVar Map.empty names
  where putVar scope name = do var <- mkVar name
                               return (Map.insert name var scope)

getZ3Var :: Id -> Scope -> Z3Var
-- lookup a z3 variable in the scope
getZ3Var name scope = case Map.lookup name scope of
  Just x  -> x
  Nothing -> error $ "Variable " ++ show name ++ " not in scope"

-- | Compilation (IMP AST -> Z3 AST)

op :: Monad m => (t1 -> t2 -> m b) -> m t1 -> m t2 -> m b
op f a b = join (liftM2 f a b)

aexp :: Scope -> AExp -> Z3 AST
aexp scope = \case
  Lit n        -> Z3.mkBvNum bitSize n
  Var name     -> return . fromJust $ Map.lookup name scope
  e1 :+: e2 -> op Z3.mkBvadd (aexp scope e1) (aexp scope e2)
  e1 :-: e2 -> op Z3.mkBvsub (aexp scope e1) (aexp scope e2)
  e1 :*: e2 -> op Z3.mkBvmul (aexp scope e1) (aexp scope e2)
  e1 :/: e2 -> op Z3.mkBvsdiv (aexp scope e1) (aexp scope e2) -- signed

bexp :: Scope -> BExp -> Z3 AST
bexp scope = \case
  True'         -> Z3.mkBool True
  False'        -> Z3.mkBool False
  e1 :<=: e2 -> op Z3.mkBvsle (aexp scope e1) (aexp scope e2) -- signed
  e1 :==: e2 -> op Z3.mkEq (aexp scope e1) (aexp scope e2)
  b1 :|: b2  -> do b1' <- bexp scope b1
                   b2' <- bexp scope b2
                   Z3.mkOr [b1', b2']
  b1 :&: b2  -> do b1' <- bexp scope b1
                   b2' <- bexp scope b2
                   Z3.mkAnd [b1', b2']
  Not b         -> Z3.mkNot =<< bexp scope b

stmt :: Scope -> Stmt -> Z3 Scope
-- takes a scope and a program, unrolls all loops, compiles into a Z3 computation
--  and returns the final scope
stmt initialScope = compile initialScope . unrollLoops
  where compile scope = \case
          Skip          -> return scope
          Set name val  -> do newVal <- aexp scope val
                              newVar <- mkVar name
                              Z3.assert =<< Z3.mkEq newVar newVal
                              return $ Map.insert name newVar scope
          Seq s1 s2     -> do scope'  <- compile scope s1
                              compile scope' s2
          If cond s1 s2 -> do cond'   <- bexp scope cond
                              scope'  <- compile scope s1
                              scope'' <- compile scope s2
                              -- consider both execution branches
                              mkIte cond' scope scope' scope''
          _             -> error "Loops have to be unrolled before compiling to SMT"

        -- returns a program with all loops unrolled exactly `unrollTimes` times
        unrollLoops = \case
          While cond body -> unroll unrollTimes (While cond body)
          s1 `Seq` s2   -> unrollLoops s1 `Seq` unrollLoops s2
          If cond s1 s2 -> If cond (unrollLoops s1) (unrollLoops s2)
          c             -> c -- non-recursive statements are left unchanged
          where unroll 0 _                      = Skip
                unroll n loop@(While cond body) = If cond (body `Seq` unroll (n - 1) loop) Skip
                unroll _ _                      = undefined -- should never be reached

mkIte :: AST -> Scope -> Scope -> Scope -> Z3 Scope
-- asserts new values for each variable in scope depending on which branch was taken
mkIte cond scope scopeIf scopeElse = foldM update scope $ Map.keys scope
  where update scope' name = do
          newVar     <- mkVar name
          iteVal     <- Z3.mkIte cond (getZ3Var name scopeIf) (getZ3Var name scopeElse)
          -- link the variable to its value, depending on which branch was taken
          Z3.assert =<< Z3.mkEq newVar iteVal
          return $ Map.insert name newVar scope' -- update var in scope

constrainVars :: Map Id Constraint -> Scope -> Z3 ()
-- receives a mapping with variable names and constraints and a scope with z3 variables,
--  and links the variables to their corresponding constraints
constrainVars constraints scope = forM_ (Map.toList constraints) $ \ (name, inv) -> do
  let var = scope ! name
  case inv of
    ConstrainEq n  -> Z3.assert =<< Z3.mkEq var =<< Z3.mkBvNum bitSize n
    ConstrainNeq n -> Z3.assert =<< Z3.mkNot =<< Z3.mkEq var =<< Z3.mkBvNum bitSize n
    ConstrainLte n -> Z3.assert =<< Z3.mkBvsle var =<< Z3.mkBvNum bitSize n
    ConstrainGte n -> Z3.assert =<< Z3.mkBvsge var =<< Z3.mkBvNum bitSize n

buildZ3Computation :: Map Id Constraint -> Map Id Constraint -> Stmt -> Z3 ()
-- receives input and output variable constraints and a program.
-- builds a Z3 computation from the program and the constraints.
buildZ3Computation inConstrs outConstrs program =
  do initialScope <- mkScope $ Map.keys inConstrs ++ Map.keys outConstrs
     constrainVars inConstrs initialScope
     finalScope <- stmt initialScope program
     constrainVars outConstrs finalScope
