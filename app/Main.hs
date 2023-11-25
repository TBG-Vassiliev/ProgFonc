import Formula
import qualified NormalForm as NF
import qualified Literal as Lit
import qualified Data.Set as Set

main :: IO ()
main = do
    -- Création de littéraux et de formules
    let litA = Lit.PosVar "A"
    let litB = Lit.NegVar "B"
    let form1 = Var "A"
    let form2 = Not (Var "B")
    let form3 = And form1 form2
    let form4 = Or form1 (Var "B")

    -- Affichage des formules
    putStrLn $ "Formule 1: " ++ show form1
    putStrLn $ "Formule 2: " ++ show form2
    putStrLn $ "Formule 3: " ++ show form3
    putStrLn $ "Formule 4: " ++ show form4

    -- Simplification d'une formule
    --let simplifiedForm = simplify form3
    -- putStrLn $ "Formule simplifiée: " ++ show simplifiedForm
    
    -- Conversion d'une formule en CNF et inversement
    let cnf = NF.fromFormula form4
    putStrLn $ "CNF de form4: " ++ show cnf
    let cnfToFormula = NF.toFormula cnf
    putStrLn $ "Conversion CNF en Formule: " ++ show cnfToFormula
    
    -- Vérification de tautologie
    let isTautologyForm1 = tautology form1
    putStrLn $ "form1 est-elle une tautologie? " ++ show isTautologyForm1

    -- Application de la règle de Robinson
    let robinsonApplied = NF.robinson cnf
    putStrLn $ "CNF après application de la règle de Robinson: " ++ show robinsonApplied
    



-- Pour supprimer les fichier de complations à la fin dans src/app :
-- rm Formula.o Formula.hi Literal.o Literal.hi NormalForm.o NormalForm.hi Main.o Main.hi haskell

-- Pour compiler
-- ghc -o haskell -i/home/cytech/Desktop/ProjetHaskell/src/ Main.hs
