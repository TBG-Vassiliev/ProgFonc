{- |
  Module : NormalForm
  Description : A module representing conjunctive normal forms
  Maintainer  : ???
-}
module NormalForm(CNF(..), size, toFormula, fromFormula, robinson) where

import Data.Set (Set)

import Formula (Formula)
import Literal (Literal)

-- | A conjunctive normal form
newtype CNF = CNF (Set (Set Literal))
--  deriving Eq
-- !!! Once type 'Literal' derive typeclass 'Eq', implementation MUST derive typeclass 'Eq' so the previous line of code will have to be uncommented.
-- NB: 'newtype' is an optimized declaration of 'data' for types with a sole constructor with a sole parameter (i.e. encapsulation).

instance Show CNF where
  show _ = undefined -- TODO

-- | Size (number of literals)
size :: CNF -> Int
size (CNF clauses) = sum (map Set.size (Set.toList clauses))

-- | Convert normal form to logical formula
toFormula :: CNF -> Formula
toFormula (CNF clauses) = foldr conj (fromBool True) (map toDisjunction (Set.toList clauses))
  where
    toDisjunction :: Set Literal -> Formula
    toDisjunction literals = foldr disj (fromBool False) (Set.toList literals)

-- | Convert logical formula to normal form
fromFormula :: Formula -> CNF
fromFormula formula = CNF (Set.singleton (toLiterals formula))
  where
    toLiterals :: Formula -> Set Literal
    toLiterals (Var var)     = Set.singleton (PosVar var)
    toLiterals (Not (Var var)) = Set.singleton (NegVar var)
    toLiterals (Or f1 f2)    = Set.union (toLiterals f1) (toLiterals f2)
    toLiterals _             = Set.empty

-- | Apply ROBINSON's rule on clauses
robinson :: CNF -> CNF
robinson _ = undefined -- TODO

