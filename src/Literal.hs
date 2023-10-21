{-# LANGUAGE KindSignatures, DeriveGeneric #-}

module Literal
  ( Literal(..),
    fromBool,
    fromPositive,
    fromNegative,
    neg,
    toFormula
  ) where

import Data.Kind (Type)
import GHC.Generics (Generic)
import Formula (Formula(..))  -- Ceci importe le type Formula du module Formula. Vérifiez que le chemin est correct.

-- | 'Literal' représente un élément de base dans les formules logiques.
-- Il peut être soit une constante booléenne ('Constant'), soit une variable positive ('Positive'), soit une variable négative ('Negative').
data Literal :: Type where
  Constant :: Bool -> Literal       -- ^ Représente une constante booléenne (vrai ou faux).
  Positive :: String -> Literal     -- ^ Représente une variable logique non niée.
  Negative :: String -> Literal     -- ^ Représente la négation d'une variable logique.
  deriving (Eq, Ord, Show, Generic) -- ^ Derive les instances standard pour 'Literal', y compris la comparaison, la sérialisation, et la conversion en chaîne.

-- | 'fromBool' convertit une valeur booléenne en un 'Literal' constant.
fromBool :: Bool -> Literal
fromBool = Constant

-- | 'fromPositive' crée un 'Literal' positif à partir d'une chaîne de caractères représentant une variable.
fromPositive :: String -> Literal
fromPositive = Positive

-- | 'fromNegative' crée un 'Literal' négatif à partir d'une chaîne de caractères représentant une variable.
fromNegative :: String -> Literal
fromNegative = Negative

-- | 'neg' négocie un 'Literal'. Si le 'Literal' est une constante, il inverse sa valeur booléenne.
-- Si c'est une variable positive, il devient négatif, et vice versa.
neg :: Literal -> Literal
neg (Constant b) = Constant (not b)  -- Inverse la valeur booléenne.
neg (Positive s) = Negative s        -- Convertit une variable positive en négative.
neg (Negative s) = Positive s        -- Convertit une variable négative en positive.

-- | 'toFormula' convertit un 'Literal' en une formule logique.
-- Les constantes restent des constantes, les variables positives deviennent des variables dans la formule,
-- et les variables négatives sont représentées comme la négation des variables dans la formule.
toFormula :: Literal -> Formula
toFormula (Constant b) = Constant b  -- Reste une constante dans la formule.
toFormula (Positive s) = Var s       -- Devient une variable dans la formule.
toFormula (Negative s) = Not (Var s) -- Devient la négation d'une variable dans la formule.
