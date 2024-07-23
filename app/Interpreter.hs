module Interpreter where

import Data.Int (Int32)
import Data.String (IsString (..))

-- | Types

-- Variable Identifier
newtype Id = Id String deriving (Eq, Ord)
instance Show Id where show (Id s) = s
instance IsString Id where fromString = Id -- enables `name :: Id ; name = "x"`

-- Variable scope as a list of pairs (variable name, value)
type Scope = [(Id, Int32)]

-- Arithmetic Expressions
data AExp
  = Lit Int32
  | Var Id
  | AExp :+: AExp -- infix constructor `+`
  | AExp :-: AExp
  | AExp :*: AExp
  | AExp :/: AExp
  deriving (Show, Eq)

-- Boolean Expressions
data BExp
  = True'
  | False'
  | AExp :<=: AExp
  | AExp :==: AExp
  | BExp :|: BExp
  | BExp :&: BExp
  | Not BExp
  deriving (Show, Eq)

-- Statements
data Stmt
  = Skip               -- no-op
  | Set Id AExp       -- variable assignment
  | Seq Stmt Stmt      -- consecutive statements
  | If BExp Stmt Stmt -- if-then-else
  | While BExp Stmt   -- while loop
  deriving (Show, Eq)

-- | Interpreter

evalAExp :: Scope -> AExp -> Maybe Int32
evalAExp _ (Lit i)         = Just i
evalAExp scope (Var name)  = lookup name scope
evalAExp scope (e1 :+: e2) = (+) <$> evalAExp scope e1 <*> evalAExp scope e2
evalAExp scope (e1 :-: e2) = (-) <$> evalAExp scope e1 <*> evalAExp scope e2
evalAExp scope (e1 :*: e2) = (*) <$> evalAExp scope e1 <*> evalAExp scope e2
evalAExp scope (e1 :/: e2) = div <$> evalAExp scope e1 <*> evalAExp scope e2

evalBExp :: Scope -> BExp -> Maybe Bool
evalBExp _ True'            = Just True
evalBExp _ False'           = Just False
evalBExp scope (e1 :<=: e2) = (<=) <$> evalAExp scope e1 <*> evalAExp scope e2
evalBExp scope (e1 :==: e2) = (==) <$> evalAExp scope e1 <*> evalAExp scope e2
evalBExp scope (e1 :|: e2)  = (||) <$> evalBExp scope e1 <*> evalBExp scope e2
evalBExp scope (e1 :&: e2)  = (&&) <$> evalBExp scope e1 <*> evalBExp scope e2
evalBExp scope (Not e)      = not <$> evalBExp scope e

evalStmt :: Scope -> Stmt -> Maybe Scope
evalStmt scope Skip                   = Just scope
evalStmt scope (Set name val)         = set <$> evalAExp scope val
  where set int = (name, int) : filter (\ (var, _) -> var /= name) scope
evalStmt scope (Seq s1 s2)            = do scope' <- evalStmt scope s1
                                           evalStmt scope' s2
evalStmt scope (If cond s1 s2)        = do res <- evalBExp scope cond
                                           if res then evalStmt scope s1
                                                  else evalStmt scope s2
evalStmt scope loop@(While cond body) = do res <- evalBExp scope cond
                                           if res then evalStmt scope (Seq body loop)
                                                  else return scope
