module NormalForm
  ( CNF,
    toFormula,
    fromFormula,
    isCNF,
    toCNF
  ) where

import Formula (Formula(..))  -- Ici, on importe seulement les éléments nécessaires de Formula
import Literal (Literal(..))  -- Ici, on importe Literal depuis le bon module
import qualified Data.Set as Set
import Data.Set (Set)
import Data.Set (fromList, toList)
import Control.Monad (guard)

newtype CNF = CNF (Set (Set Literal)) deriving (Eq, Show)

toFormula :: CNF -> Formula
toFormula (CNF sets) 
  | Set.null sets = Constant True  -- CNF vide est une tautologie
  | otherwise = foldr1 And [foldr1 Or [literalToFormula lit | lit <- Set.toList clause] | clause <- Set.toList sets]
  where
    literalToFormula :: Literal -> Formula
    literalToFormula (Positive var) = Var var
    literalToFormula (Negative var) = Not (Var var)

fromFormula :: Formula -> Either String CNF
fromFormula formula
  | isCNF formula = Right (formulaToCNF formula)
  | otherwise     = Left "La formule donnée n'est pas en CNF et la conversion automatique n'est pas encore supportée."
  where
    formulaToCNF :: Formula -> CNF
    formulaToCNF (And f1 f2) = 
      let CNF s1 = formulaToCNF f1
          CNF s2 = formulaToCNF f2
      in CNF (Set.union s1 s2)
    formulaToCNF (Or f1 f2) = 
      let CNF s1 = formulaToCNF f1
          CNF s2 = formulaToCNF f2
      in CNF (Set.fromList [Set.union clause1 clause2 | clause1 <- Set.toList s1, clause2 <- Set.toList s2])
    formulaToCNF (Not (Var var)) = CNF (Set.singleton (Set.singleton (Negative var)))
    formulaToCNF (Var var) = CNF (Set.singleton (Set.singleton (Positive var)))
    formulaToCNF _ = error "Invalid formula in formulaToCNF."

isCNF :: Formula -> Bool
isCNF (Var _) = True
isCNF (Not (Var _)) = True
isCNF (And f1 f2) = isCNF f1 && isCNF f2
isCNF (Or f1 f2) = isLiteral f1 && isLiteral f2
isCNF _ = False

isLiteral :: Formula -> Bool
isLiteral (Var _) = True
isLiteral (Not (Var _)) = True
isLiteral _ = False

toCNF :: Formula -> CNF
toCNF formula
  | isCNF formula = fromFormula formula
  | otherwise = case distributeOrs formula of
    Right cnf -> cnf
    Left errMsg -> error errMsg

-- Distribuer les disjonctions sur les conjonctions
distributeOrs :: Formula -> Either String CNF
distributeOrs (And f1 f2) = do
  CNF cnf1 <- distributeOrs f1
  CNF cnf2 <- distributeOrs f2
  return $ CNF (Set.union cnf1 cnf2)
distributeOrs (Or f1 f2) = do
  CNF cnf1 <- distributeOrs f1
  CNF cnf2 <- distributeOrs f2
  return $ CNF (fromList [Set.union clause1 clause2 | clause1 <- toList cnf1, clause2 <- toList cnf2])
distributeOrs (Not (Var var)) = Right (CNF (Set.singleton (Set.singleton (Negative var))))
distributeOrs (Var var) = Right (CNF (Set.singleton (Set.singleton (Positive var))))
distributeOrs _ = Left "La formule donnée n'est pas en CNF et la conversion automatique n'est pas encore supportée."

