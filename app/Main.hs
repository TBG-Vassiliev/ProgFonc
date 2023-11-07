module Main where

import Formula
import NormalForm
import Literal
import qualified Data.Map as Map

main :: IO ()
main = do
    -- Définition de l'environnement
    let env = Map.fromList [("p", True), ("q", False)]

    -- Création d'une formule
    let formula1 = And (Or (Var "p") (Var "q")) (Not (Var "q"))

    -- Évaluation de la formule
    let result = evaluate env formula1
    putStrLn $ "L'évaluation de la formule est : " ++ show result

    -- Simplification de la formule
    let simplified = simplify formula1
    putStrLn $ "La formule simplifiée est : " ++ show simplified

    -- Vérification si la formule est une tautologie
    -- Note: Vous devrez implémenter la logique pour 'isTautology'
    let tautologyCheck = isTautology formula1
    putStrLn $ "La formule est-elle une tautologie ? " ++ show tautologyCheck

    -- Conversion d'une formule en CNF
    -- Note: Vous devrez implémenter la logique pour 'toCNF'
    let cnf = toCNF formula1
    putStrLn $ "La forme CNF de la formule est : " ++ show cnf

-- Charger le main pour windows :
-- ghci src/Literal.hs src/Formula.hs src/NormalForm.hs app/Main.hs

-- Charger le main pour Ubuntu :
-- ghc -i../src Main.hs -o aa && ./aa

