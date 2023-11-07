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
size _ = undefined -- TODO

-- | Convert normal form to logical formula
toFormula :: CNF -> Formula
toFormula _ = undefined -- TODO

-- | Convert logical formula to normal form
fromFormula :: Formula -> CNF
fromFormula _ = undefined -- TODO

-- | Apply ROBINSON's rule on clauses
robinson :: CNF -> CNF
robinson _ = undefined -- TODO

