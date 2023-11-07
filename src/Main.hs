module Main where

import Formula
import qualified NormalForm as NF
import qualified Literal as Lit
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
    let tautologyCheck = isTautology formula1
    putStrLn $ "La formule est-elle une tautologie ? " ++ show tautologyCheck

    -- Conversion d'une formule en CNF
    let cnfResult = NF.fromFormula formula1
    case cnfResult of
      Right cnf -> do
        putStrLn $ "La forme CNF de la formule est : " ++ show cnf
        -- Convertir la CNF de retour en Formula pour l'afficher
        let cnfAsFormula = NF.toFormula cnf
        putStrLn $ "La forme CNF comme Formula est : " ++ show cnfAsFormula
      Left error -> putStrLn $ "Erreur lors de la conversion en CNF: " ++ error


-- Pour supprimer les fichier de complations à la fin :
-- rm Formula.o Formula.hi Literal.o Literal.hi NormalForm.o NormalForm.hi Main.o Main.hi haskell

