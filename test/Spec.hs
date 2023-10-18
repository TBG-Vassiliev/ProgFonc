-- Ce module définit les tests spécifiques pour les fonctionnalités présentes dans le module 'NormalForm'.
module Spec where

import Test.Hspec
import NormalForm (fromFormula, toFormula, CNF(..), size)
import Formula (Formula(..))
import Literal (Literal(..))
import qualified Data.Set as Set  -- Importe Data.Set pour utiliser 'fromList'.

-- | Point d'entrée principal pour les tests. Exécute les spécifications de test définies ci-dessous.
main :: IO ()
main = hspec spec

-- | Spécifications de test pour les fonctionnalités du module 'NormalForm'.
spec :: Spec
spec = do
  -- Groupe de tests pour le module 'NormalForm'.
  describe "NormalForm" $ do

    -- Test vérifiant la conversion correcte d'une 'Formula' en 'CNF'.
    it "converts from Formula to CNF correctly" $ do
      let formula = And [Or [Var "p", Var "q"], Or [Var "q", Not (Var "r")]]
      let expectedCNF = CNF $ Set.fromList [Set.fromList [Pos "p", Pos "q"], Set.fromList [Pos "q", Neg "r"]]
      fromFormula formula `shouldBe` Right expectedCNF

    -- Test vérifiant la conversion correcte d'un 'CNF' en 'Formula'.
    it "converts from CNF to Formula correctly" $ do
      let cnf = CNF $ Set.fromList [Set.fromList [Pos "p", Pos "q"], Set.fromList [Pos "q", Neg "r"]]
      let expectedFormula = And [Or [Var "p", Var "q"], Or [Var "q", Not (Var "r")]]
      toFormula cnf `shouldBe` expectedFormula

    -- Test vérifiant que la fonction 'size' calcule correctement la taille d'un 'CNF' (nombre de littéraux).
    it "calculates the size of a CNF correctly" $ do
      let cnf = CNF $ Set.fromList [Set.fromList [Pos "p", Pos "q"], Set.fromList [Pos "q", Neg "r"]]
      size cnf `shouldBe` 4  -- Il y a 4 littéraux au total.

-- Pour exécuter ces tests, utilisez la commande 'stack test' ou 'cabal test' si vous utilisez Stack ou Cabal respectivement. 
-- C'est essentiel pour pratiquer le développement guidé par les tests (TDD) et pour s'assurer que les modifications dans le code 
-- n'introduisent pas de nouveaux bogues.
