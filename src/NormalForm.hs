{-# LANGUAGE OverloadedStrings #-}

-- | Le module 'NormalForm' offre des fonctionnalités pour manipuler les formules logiques
-- sous la forme normale conjonctive (CNF), y compris la conversion depuis et vers les formules logiques,
-- vérifier si une formule est en CNF, et calculer la taille d'une CNF.
module NormalForm (
    CNF(..), 
    toFormula, 
    fromFormula, 
    size, 
    isCNF,
    robinson  -- Ceci est un emplacement réservé pour une fonction future.
) where

import Data.Set (Set)
import qualified Data.Set as Set
import Formula (Formula(..)) -- Importe le type de données 'Formula'.
import Literal (Literal(..), neg) -- Importe le type de données 'Literal' et la fonction 'neg'.

-- | 'CNF' représente une forme normale conjonctive, qui est une conjonction de disjonctions de littéraux.
newtype CNF = CNF (Set (Set Literal)) deriving (Eq, Show) 

-- | 'toFormula' convertit une CNF en une formule logique.
toFormula :: CNF -> Formula
toFormula (CNF sets) = And [Or [literalToFormula lit | lit <- Set.toList clause] | clause <- Set.toList sets]
  where
    literalToFormula :: Literal -> Formula
    literalToFormula (Pos s) = Var s
    literalToFormula (Neg s) = Not (Var s)

-- | 'fromFormula' tente de convertir une formule logique en CNF, en supposant que la formule d'entrée est déjà en CNF.
fromFormula :: Formula -> Either String CNF
fromFormula formula =
    if isCNF formula
    then Right . CNF . Set.fromList . map (Set.fromList . map formulaToLiteral . getLiterals) $ getClauses formula
    else Left "La formule n'est pas en CNF."
  where
    getClauses, getLiterals :: Formula -> [Formula]
    getClauses (And clauses) = clauses
    getClauses _ = []

    getLiterals (Or lits) = lits
    getLiterals _ = []

    formulaToLiteral :: Formula -> Literal
    formulaToLiteral (Var s) = Pos s
    formulaToLiteral (Not (Var s)) = Neg s
    formulaToLiteral _ = error "Type de formule inattendu."

-- | 'isCNF' vérifie si une formule est en CNF.
isCNF :: Formula -> Bool
isCNF (And clauses) = all isClause clauses
isCNF _ = False

-- | 'isClause' vérifie si une formule est une clause valide dans une CNF.
isClause :: Formula -> Bool
isClause (Or literals) = all isLiteral literals
isClause _ = False

-- | 'isLiteral' vérifie si une formule est un littéral valide.
isLiteral :: Formula -> Bool
isLiteral (Var _) = True
isLiteral (Not (Var _)) = True
isLiteral _ = False

-- | 'size' retourne le nombre total de littéraux dans une CNF.
size :: CNF -> Int
size (CNF sets) = sum (map Set.size (Set.toList sets))

-- | 'robinson' est un emplacement réservé pour l'algorithme d'unification de Robinson.
-- Cela devrait être remplacé par une implémentation réelle de l'algorithme.
robinson :: CNF -> CNF
robinson = error "Non implémenté : l'algorithme de Robinson nécessite une logique spécifique."
