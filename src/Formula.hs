{-# LANGUAGE DeriveGeneric #-}

module Formula
  ( Formula(..),
    Environment,
    evaluate,
    (<=>),
    simplify,
    isTautology,
    isContradiction,
    toCNF
  ) where

import Data.Set (Set)
import qualified Data.Set as Set
import Data.Map (Map)
import qualified Data.Map as Map
import GHC.Generics (Generic)
import Control.Applicative (liftA2)

-- `Formula` est un type de données qui représente une formule logique.
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
    Constant b -> Just b  -- Une constante est évaluée comme sa propre valeur.
    Var x      -> Map.lookup x env  -- Une variable prend la valeur dans l'environnement.
    Not f      -> not <$> evaluate env f  -- La négation inverse la valeur de la formule.
    And f1 f2  -> liftA2 (&&) (evaluate env f1) (evaluate env f2)  -- L'opérateur "et" logique.
    Or f1 f2   -> liftA2 (||) (evaluate env f1) (evaluate env f2)  -- L'opérateur "ou" logique.
    Implies f1 f2 -> liftA2 (<=) (fmap not (evaluate env f1)) (evaluate env f2)  -- L'implication logique.
    Equivalent f1 f2 -> (==) <$> evaluate env f1 <*> evaluate env f2  -- L'équivalence logique.

-- `simplify` simplifie une formule logique en appliquant des règles de simplification de base.
simplify :: Formula -> Formula
-- Le code ici applique des transformations spécifiques pour simplifier les formules.

-- `isTautology` vérifie si une formule est une tautologie (toujours vraie).
isTautology :: Formula -> Bool
isTautology f = all (\env -> evaluate env f == Just True) (generateEnvironments f)

-- `isContradiction` vérifie si une formule est une contradiction (toujours fausse).
isContradiction :: Formula -> Bool
isContradiction f = all (\env -> evaluate env f == Just False) (generateEnvironments f)

-- `toCNF` convertit une formule en forme normale conjonctive.
toCNF :: Formula -> Formula
-- Le code ici transforme d'abord la formule en sa forme normale négative, puis en FNC.

-- D'autres fonctions auxiliaires, comme `eliminateImplications`, `toNNF`, `nnfToCNF`, etc., aident dans ces transformations.
-- La fonction `generateEnvironments` génère tous les environnements possibles (valuations) pour une formule donnée, en attribuant toutes les combinaisons possibles de vrai/faux aux variables de la formule.

-- L'opérateur `<=>` vérifie si deux formules sont logiquement équivalentes, c'est-à-dire qu'elles ont les mêmes valeurs de vérité dans tous les environnements possibles.
(<=>) :: Formula -> Formula -> Bool
f1 <=> f2 = all (\env -> evaluate env f1 == evaluate env f2) (generateEnvironments (Or f1 f2))
