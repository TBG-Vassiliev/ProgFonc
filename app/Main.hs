import Formula
import qualified NormalForm as NF
import qualified Literal as Lit
import qualified Data.Set as Set


main :: IO ()
main = do
    -- Entrer la formule
    let formula = Or (Var "A") (Not (Var "A")) -- A ∨ ¬A

    -- Affichage des formules
    putStrLn $ "Formule : " ++ show formula

    -- Simplification d'une formule
    let simplifiedForm = simplify formula
    putStrLn $ "Formule simplifiée : " ++ show simplifiedForm
    
    -- -- Conversion d'une formule en CNF et inversement
    let cnf = NF.fromFormula formula
    putStrLn $ "CNF de formula : " ++ show cnf

    let cnfToFormula = NF.toFormula cnf
    putStrLn $ "Conversion CNF en Formule: " ++ show cnfToFormula
    
    -- -- Vérification de tautologie
    let isTautology = tautology formula
    putStrLn $ "La formule entrée est-elle une tautologie ? " ++ show isTautology

    -- -- Application de la règle de Robinson
    let robinsonApplied = NF.robinson cnf
    putStrLn $ "CNF après application de la règle de Robinson: " ++ show robinsonApplied
  

-- Pour supprimer les fichier de complations à la fin dans src :
-- Executer le fichier clean.exe

-- Pour compiler
-- ghc -o logical Main.hs
