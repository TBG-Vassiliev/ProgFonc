module Main where

import NormalForm (fromFormula, toFormula, CNF(..)) -- Importez les fonctions nécessaires de votre module
import Formula (Formula(..)) -- Supposons que vous avez un type Formula défini quelque part
import Literal (Literal(..)) -- Supposons que vous avez un type Literal défini quelque part

-- Fonction principale qui initie l'exécution de votre programme.
main :: IO ()
main = do
    putStrLn "Début du programme de conversion de formule."
    
    -- Créez une formule de test. Vous pouvez également obtenir ceci de l'utilisateur ou d'une autre source.
    let testFormula = And [Or [Var "p", Not (Var "q")], Or [Var "q", Not (Var "r")]]
    
    -- Montrez la formule originale
    putStrLn $ "Formule originale : " ++ show testFormula

    -- Convertissez la formule en CNF et imprimez-la
    case fromFormula testFormula of
        Right cnf -> putStrLn $ "Formule en CNF : " ++ show (toFormula cnf) -- Convertit la CNF en Formula pour l'impression
        Left errorMsg -> putStrLn $ "Erreur : " ++ errorMsg

    putStrLn "Fin du programme de conversion de formule."


-- Si vous voulez lancer votre application et voir comment elle fonctionne en situation réelle, vous exécutez main. C'est utile pour voir le comportement de votre programme en production ou pour un usage général.
