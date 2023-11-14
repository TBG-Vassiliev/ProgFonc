{-# LANGUAGE KindSignatures #-}
{- |
  Module : Formula
  Description : A module representing a logical predicate with logical variables
  Maintainer  : ???
-}
module Formula(Formula(..), fromBool, fromString, neg, conj, disj, implies, isLiteral, has, size, variables, Environment, evaluate, (<=>), tautology, simplify) where

import Data.Kind

import Data.Set (Set)
import qualified Data.Set as Set hiding (Set)
import Data.Map (Map)
import qualified Data.Map as Map hiding (Map)
import Control.Applicative (liftA2)
import Control.Monad (sequence)

-- | A logical proposition may be :
-- | * a boolean constant
-- | * a logical variable
-- | * a negation of a logical proposition
-- | * a conjunction (a.k.a. "logical and") of two logical expressions
-- | * a disjunction (a.k.a. "logical  or") of two logical expressions
--  deriving Eq
-- !!! Implementation MUST derive typeclass 'Eq' so the previous line of code must be uncommented

data Formula
  = BoolConst Bool       
  | Var String         
  | Not Formula          
  | And Formula Formula       
  | Or Formula Formula        
  deriving Eq

-- CONSTRUCTORS

-- | Convert a boolean value into a constant logical formula
fromBool :: Bool -> Formula
fromBool = BoolConst

-- | Convert a variable name into a formula with only the corresponding logical variable
fromString :: String -> Formula
fromString = Var

-- | Negation operation
neg :: Formula -> Formula
neg = Not

-- | Conjunction operation (logical "and")
conj :: Formula -> Formula -> Formula
conj = And

-- | Disjunction operation (logical "or")
disj :: Formula -> Formula -> Formula
disj = Or

-- | Implies operation
implies :: Formula -> Formula -> Formula
implies a b = Or (Not a) b

-- | Is the formula literal ?
isLiteral :: Formula -> Bool
isLiteral (Var _) = True      -- A variable is a literal
isLiteral (Not (Var _)) = True  -- The negation of a variable is also a literal
isLiteral _ = False      -- Anything else is not a literal

-- | Search for logical variable in formula
has :: Formula -> String -> Bool
has (Var varName) targetVar = varName == targetVar
has (Not formula) targetVar = has formula targetVar
has (And formula1 formula2) targetVar = has formula1 targetVar || has formula2 targetVar
has (Or formula1 formula2) targetVar = has formula1 targetVar || has formula2 targetVar
has _ _ = False

-- | Size (number of operators)
size :: Formula -> Int
size (BoolConst _)   = 0
size (Var _)         = 0
size (Not formula)   = 1 + size formula
size (And f1 f2)     = 1 + size f1 + size f2
size (Or f1 f2)      = 1 + size f1 + size f2

-- | Retrieve set of all variables occuring in formula
variables :: Formula -> Set String
variables (BoolConst _)   = Set.empty
variables (Var varName)   = Set.singleton varName
variables (Not formula)   = variables formula
variables (And f1 f2)     = Set.union (variables f1) (variables f2)
variables (Or f1 f2)      = Set.union (variables f1) (variables f2)


instance Show Formula where
  show (BoolConst b)   = show b
  show (Var varName)   = varName
  show (Not formula)   = "not " ++ show formula
  show (And f1 f2)     = "(" ++ show f1 ++ " and " ++ show f2 ++ ")"
  show (Or f1 f2)      = "(" ++ show f1 ++ " or " ++ show f2 ++ ")"


-- | Environment associating logical variables to logical values
type Environment = Map String Bool

-- | Evaluation (if possible) of formula in a given environment
evaluate :: Environment -> Formula -> Maybe Bool
evaluate env (BoolConst b) = Just b
evaluate env (Var varName) = Map.lookup varName env
evaluate env (Not formula) = not <$> evaluate env formula
evaluate env (And f1 f2) = liftA2 (&&) (evaluate env f1) (evaluate env f2)
evaluate env (Or f1 f2) = liftA2 (||) (evaluate env f1) (evaluate env f2)

-- | Logical equivalence on formulae
(<=>) :: Formula -> Formula -> Bool
(BoolConst b1) <=> (BoolConst b2) = b1 == b2
(Var var1) <=> (Var var2) = var1 == var2
(Not f1) <=> (Not f2) = f1 <=> f2
(And f1a f1b) <=> (And f2a f2b) = (f1a <=> f2a) && (f1b <=> f2b)
(Or f1a f1b) <=> (Or f2a f2b) = (f1a <=> f2a) && (f1b <=> f2b)
_ <=> _ = False


-- | Is the formula a tautology ?
tautology :: Formula -> Bool
tautology formula = checkAllEnvironments (Set.toList $ variables formula)
  where
    checkAllEnvironments :: [String] -> Bool
    checkAllEnvironments [] = True
    checkAllEnvironments (var:vars) =
      let envTrue = Map.insert var True Map.empty
          envFalse = Map.insert var False Map.empty
      in evaluate envTrue formula == Just True
         && evaluate envFalse formula == Just True
         && checkAllEnvironments vars

-- | Attempts to simplify the proposition
simplify :: Formula -> Formula
simplify _ = undefined -- TODO
