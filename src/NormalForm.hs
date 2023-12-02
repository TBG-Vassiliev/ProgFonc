{- |
  Module : NormalForm
  Description : A module representing conjunctive normal forms
  Maintainer  : ???
-}
module NormalForm(CNF(..), size, toFormula, fromFormula, robinson) where

import Data.Set (Set)
import qualified Data.Set as Set

import Formula (Formula) 
import qualified Formula as F
import Literal (Literal)
import qualified Literal as L

-- | A conjunctive normal form
newtype CNF = CNF (Set (Set Literal)) deriving(Eq)
--  deriving Eq
-- !!! Once type 'Literal' derive typeclass 'Eq', implementation MUST derive typeclass 'Eq' so the previous line of code will have to be uncommented.
-- NB: 'newtype' is an optimized declaration of 'data' for types with a sole constructor with a sole parameter (i.e. encapsulation).

instance Show CNF where
  show (CNF clauses) = unwords (map showClause (Set.toList clauses))
    where
      showClause literals = "(" ++ unwords (map show (Set.toList literals)) ++ ")"

-- | Size (number of literals)
size :: CNF -> Int
size (CNF clauses) = sum (map Set.size (Set.toList clauses))

-- | Convert normal form to logical formula
toFormula :: CNF -> Formula
toFormula (CNF clauses) = foldr F.conj (F.fromBool True) (map toDisjunction (Set.toList clauses))
  where
    toDisjunction :: Set Literal -> Formula
    toDisjunction literals = foldr F.disj (F.fromBool False) (map L.toFormula (Set.toList literals))

-- | Convert logical formula to normal form
fromFormula :: Formula -> CNF
fromFormula formula = CNF (Set.singleton (toLiterals formula))
  where
    toLiterals :: Formula -> Set Literal
    toLiterals (F.Var var)     = Set.singleton (L.PosVar var)
    toLiterals (F.Not (F.Var var)) = Set.singleton (L.NegVar var)
    toLiterals (F.Or f1 f2)    = Set.union (toLiterals f1) (toLiterals f2)  
    -- toLiterals (F.And f1 f2) = Set.fromList [ Set.fromList (Set.toList l1 ++ Set.toList l2) | l1 <- toLiterals f1, l2 <- toLiterals f2 ]
    toLiterals _ = Set.empty

-- | Apply ROBINSON's rule on clauses
robinson :: CNF -> CNF
robinson (CNF clauses) = CNF (Set.fromList (map simplifyClause (resolvePairs (Set.toList clauses))))
  where
    resolvePairs :: [Set Literal] -> [Set Literal]
    resolvePairs [] = []
    resolvePairs (c:cs) = c : resolvePairs (map (resolvePair c) cs ++ cs)

    resolvePair :: Set Literal -> Set Literal -> Set Literal
    resolvePair c1 c2 = Set.union (Set.difference c1 (Set.map L.neg c2)) (Set.difference c2 (Set.map L.neg c1))

    simplifyClause :: Set Literal -> Set Literal
    simplifyClause clause = Set.filter (\l -> not (Set.member (L.neg l) clause)) clause

