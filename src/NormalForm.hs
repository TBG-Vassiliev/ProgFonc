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
toFormula (CNF clauses) = F.simplify(foldr F.And (F.fromBool True) (map toDisjunction (Set.toList clauses))) -- Le simplify permet d'éviter les 'or False' et les 'and True' inutiles
  where
    toDisjunction :: Set Literal -> Formula
    toDisjunction literals = foldr F.Or (F.fromBool False) (map L.toFormula (Set.toList literals))

-- | Convert logical formula to normal form
fromFormula :: Formula -> CNF
fromFormula f =
  let negativeDescent :: Formula -> Formula -- Simplifier la formule en éliminant les double-négations et en manipulant les négations dans les clauses.
      negativeDescent (F.BoolConst b) = F.fromBool b
      negativeDescent (F.Var v) = F.fromString v
      negativeDescent (F.Not (F.BoolConst b)) = F.BoolConst (not b)
      negativeDescent (F.Not (F.Var v)) = F.neg (F.fromString v)
      negativeDescent (F.Not (F.Not f')) = negativeDescent f'
      negativeDescent (F.Not (F.And f1 f2)) = F.Or (negativeDescent (F.Not f1)) (negativeDescent (F.Not f2))
      negativeDescent (F.Not (F.Or f1 f2)) = F.And (negativeDescent (F.Not f1)) (negativeDescent (F.Not f2))
      negativeDescent (F.And f1 f2) = F.And (negativeDescent f1) (negativeDescent f2)
      negativeDescent (F.Or f1 f2) = F.Or (negativeDescent f1) (negativeDescent f2) in
  
  let jointure :: [[a]] -> [[a]] -> [[a]] -- Combiner chaque liste de la première liste avec chaque liste de la deuxième liste
      jointure [] ys = ys
      jointure (x:xs) ys = map (x ++) ys ++ jointure xs ys in

  let distribute :: CNF -> CNF -> CNF -- Distribution des conjonctions sur les disjonctions dans la conversion en CNF
      distribute (CNF set1) (CNF set2) =
        let mat1 = Set.toList $ Set.map Set.toList set1 in
        let mat2 = Set.toList $ Set.map Set.toList set2 in
          CNF $ Set.fromList $ map Set.fromList $ jointure mat1 mat2 in


  let fromFormula' :: Formula -> CNF -- Effectuer la conversion réelle en CNF (traite les différents cas)
      fromFormula' f' = case f' of

        F.BoolConst True -> CNF Set.empty
        F.BoolConst False -> CNF $ Set.singleton Set.empty
        F.Var v -> CNF $ Set.singleton $ Set.singleton $ L.fromPositive v
        F.Not (F.BoolConst True) -> CNF $ Set.singleton Set.empty
        F.Not (F.BoolConst False) -> CNF Set.empty
        F.Not (F.Var v) -> CNF $ Set.singleton $ Set.singleton $ L.fromNegative v
        F.Not (F.Not f'') -> fromFormula' f''
        F.Not _ -> error "fromFormula: negative descent failed"
        
        F.And f1 f2 -> let CNF set1 = fromFormula' f1 in
          let CNF set2 = fromFormula' f2 in
            CNF (Set.union set1 set2)
        
        F.Or f1 f2 -> let CNF set1 = fromFormula' f1 in
          let CNF set2 = fromFormula' f2 in
            distribute (CNF set1) (CNF set2)
      in

    fromFormula' (negativeDescent f)

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

