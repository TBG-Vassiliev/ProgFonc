{-# LANGUAGE GADTs, KindSignatures, DeriveGeneric #-}

-- Le module Literal définit la structure et les opérations pour les littéraux dans les formules logiques.
module Literal
  ( Literal(..),  -- Expose le type `Literal`
    neg,          -- Fonction pour négocier un littéral
    toFormula     -- Fonction pour convertir un littéral en une formule
  ) where

import Data.Kind (Type)
import GHC.Generics (Generic)
import Formula (Formula(..))  -- Importe le type `Formula` d'un autre module, supposé avoir des constructeurs comme Var et Not

-- | Type de données `Literal` avec des constructeurs pour les littéraux positifs et négatifs.
-- Utilise GADTs pour plus de flexibilité et de sécurité de type.
data Literal :: Type where
  Positive :: String -> Literal  -- ^ Constructeur pour les littéraux positifs
  Negative :: String -> Literal  -- ^ Constructeur pour les littéraux négatifs
  deriving (Eq, Ord, Show, Generic)  -- Les instances courantes sont dérivées pour `Literal`.

-- | La fonction 'neg' inverse la polarité d'un littéral.
-- Si c'est positif, cela devient négatif et vice versa.
neg :: Literal -> Literal
neg (Positive s) = Negative s  -- Inverse un littéral positif
neg (Negative s) = Positive s  -- Inverse un littéral négatif

-- | La fonction 'toFormula' convertit un littéral en une formule logique.
-- Les littéraux positifs deviennent des variables, les négatifs deviennent la négation des variables.
toFormula :: Literal -> Formula
toFormula (Positive s) = Var s          -- Convertit un littéral positif en une variable de formule
toFormula (Negative s) = Not (Var s)    -- Convertit un littéral négatif en négation de variable de formule
