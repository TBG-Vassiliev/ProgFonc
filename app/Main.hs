module Main (main) where

import Formula
import qualified Formula as F
import Literal
import qualified Literal as L
import NormalForm
import qualified NormalForm as NF


main :: IO ()
main = 

    complexFormula :: Formula
complexFormula =
  let
    a = Var "A"
    b = Var "B"
    c = Var "C"
    d = Var "D"
    e = Var "E"
    f = Var "F"
    g = Var "G"
  in
    Or
      (And a (Not b))
      (Or
        (And (Not c) d)
        (And e (Or f (Not g)))
      )
    
    putStrLn F.tautology(complexFormula)
