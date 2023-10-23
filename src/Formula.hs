{-# LANGUAGE DeriveGeneric #-}

module Formula
  ( Formula(..),
    Environment,
    evaluate,
    simplify,
    isTautology,
    isContradiction,
    getVars,
    (<=>)  -- Équivalence logique
  ) where

import Control.Applicative (liftA2)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Map (Map)
import qualified Data.Map as Map
import GHC.Generics (Generic)

-- `Formula` représente une formule logique.
data Formula
  = Constant Bool                -- Constante booléenne : True ou False
  | Var String                   -- Variable propositionnelle
  | Not Formula                  -- Négation logique
  | And Formula Formula          -- Conjonction logique (ET)
  | Or Formula Formula           -- Disjonction logique (OU)
  | Implies Formula Formula      -- Implication logique
  | Equivalent Formula Formula   -- Équivalence logique
  deriving (Eq, Show, Generic)

-- `Environment` est un mappage des noms de variables aux valeurs booléennes.
type Environment = Map String Bool

-- `evaluate` évalue une formule logique dans un environnement donné.
evaluate :: Environment -> Formula -> Maybe Bool
evaluate env formula = 
  case formula of
    Constant b -> Just b
    Var x      -> Map.lookup x env
    Not f      -> not <$> evaluate env f
    And f1 f2  -> liftA2 (&&) (evaluate env f1) (evaluate env f2)
    Or f1 f2   -> liftA2 (||) (evaluate env f1) (evaluate env f2)
    Implies f1 f2 -> liftA2 (<=) (fmap not (evaluate env f1)) (evaluate env f2)
    Equivalent f1 f2 -> (==) <$> evaluate env f1 <*> evaluate env f2

-- `simplify` simplifie une formule logique.
simplify :: Formula -> Formula
simplify formula = 
  case formula of
    Not (Not f) -> simplify f
    And f1 f2   -> And (simplify f1) (simplify f2)
    Or f1 f2    -> Or (simplify f1) (simplify f2)
    -- autres cas de simplification ici
    _           -> formula  -- cas par défaut, aucune simplification

-- `getVars` extrait toutes les variables uniques d'une formule.
getVars :: Formula -> Set String
getVars formula =
  case formula of
    Var x      -> Set.singleton x
    Not f      -> getVars f
    And f1 f2  -> getVars f1 `Set.union` getVars f2
    Or f1 f2   -> getVars f1 `Set.union` getVars f2
    Implies f1 f2 -> getVars f1 `Set.union` getVars f2
    Equivalent f1 f2 -> getVars f1 `Set.union` getVars f2
    _          -> Set.empty  -- cas par défaut, aucune variable

-- `isTautology` vérifie si une formule est une tautologie.
isTautology :: Formula -> Bool
isTautology formula = all (\env -> evaluate env formula == Just True) possibleEnvs
  where
    vars = getVars formula
    possibleEnvs = map Map.fromList $ sequence [[(var, b) | b <- [True, False]] | var <- Set.toList vars]

-- `isContradiction` vérifie si une formule est une contradiction.
isContradiction :: Formula -> Bool
isContradiction formula = all (\env -> evaluate env formula == Just False) possibleEnvs
  where
    vars = getVars formula
    possibleEnvs = map Map.fromList $ sequence [[(var, b) | b <- [True, False]] | var <- Set.toList vars]

-- L'opérateur `<=>` vérifie si deux formules sont équivalentes.
(<=>) :: Formula -> Formula -> Bool
f1 <=> f2 = all (\env -> evaluate env f1 == evaluate env f2) possibleEnvs
  where
    vars = getVars f1 `Set.union` getVars f2
    possibleEnvs = map Map.fromList $ sequence [[(var, b) | b <- [True, False]] | var <- Set.toList vars]
