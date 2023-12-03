import Formula
import qualified NormalForm as NF
import qualified Literal as Lit
import qualified Data.Set as Set


main :: IO ()
main = do
    -- Entrer la formule
    let formula = (Or (And (Var "A") (Or (Var "B") (Var "C"))) (Var "D"))
    -- let formula = Or (And (Var "A") (Var "B")) (And (Not (Var "C")) (Or (Var "A") (Var "B"))) -- (A∧B)∨(¬C∧(A∨B))

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

    -- Application de la règle de Robinson
    let robinsonApplied = NF.robinson cnf
    putStrLn $ "CNF après application de la règle de Robinson: " ++ show robinsonApplied

    -- Créer quelques formules pour vérifier si elles sont équivalentes : (<=>)
    let formula1 = Or (And (Var "A") (Var "B")) (Var "C")
    let formula2 = And (Or (Var "A") (Var "C")) (Or (Var "B") (Var "C"))
    putStrLn $ "Formula 1: " ++ show formula1
    putStrLn $ "Formula 2: " ++ show formula2
    let result = (<=>) formula1 formula2
    putStrLn $ "Les formules sont équivalentes ? " ++ show result
  

-- Pour supprimer les fichier de complations à la fin dans src :
-- Executer le fichier clean.exe

-- Pour compiler
-- ghc src/Formula.hs src/Literal.hs src/NormalForm.hs app/Main.hs -o logical
