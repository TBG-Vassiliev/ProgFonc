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

-- | A logical proposition may be :
-- | * a boolean constant
-- | * a logical variable
-- | * a negation of a logical proposition
-- | * a conjunction (a.k.a. "logical and") of two logical expressions
-- | * a disjunction (a.k.a. "logical  or") of two logical expressions
-- data Formula :: Type -- TODO
--  deriving Eq
-- !!! Implementation MUST derive typeclass 'Eq' so the previous line of code must be uncommented

data Formula
  = BoolConst Bool       
  | Var String         
  | Not Formula          
  | And Formula Formula       
  | Or Formula Formula        
  deriving (Eq, Show)

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
size _ = undefined -- TODO

-- | Retrieve set of all variables occuring in formula
variables :: Formula -> Set String
variables _ = undefined -- TODO

instance Show Formula where
  show _ = undefined -- TODO



-- | Environment associating logical variables to logical values
type Environment = Map String Bool

-- | Evaluation (if possible) of formula in a given environment
evaluate :: Environment -> Formula -> Maybe Bool
evaluate _ _ = undefined -- TODO

-- | Logical equivalence on formulae
(<=>) :: Formula -> Formula -> Bool
_ <=> _ = undefined -- TODO

-- | Is the formula a tautology ?
tautology :: Formula -> Bool
tautology _ = undefined -- TODO

-- | Attempts to simplify the proposition
simplify :: Formula -> Formula
simplify _ = undefined -- TODO
