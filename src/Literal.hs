{-# LANGUAGE KindSignatures #-}
{- |
  Module : Literal
  Description : A module representing a literal element in a normal form
  Maintainer  : ???
-}
module Literal(Literal(..), fromBool, fromPositive, fromNegative, neg, toFormula) where

import Data.Kind

import Formula (Formula)

-- | A literal element may be :
-- | * a boolean constant
-- | * a logical variable or the negation of it
data Literal = PosVar String | NegVar String deriving (Eq, Ord)
--  deriving (Eq, Ord)
-- !!! Implementation MUST derive typeclasses 'Eq' and 'Ord' so the previous line of code must be uncommented

instance Show
 Literal where
  show (PosVar var) = var
  show (NegVar var) = "not " ++ var

-- | Convert boolean value to constant literal
fromBool :: Bool -> Literal
fromBool True  = PosVar "True"
fromBool False = PosVar "False"

-- | Convert logical variable to a positive literal
fromPositive :: String -> Literal
fromPositive var = PosVar var

-- | Convert logical variable to a positive literal
fromNegative :: String -> Literal
fromNegative var = NegVar var

-- | Negation operation
neg :: Literal -> Literal
neg (PosVar var) = NegVar var
neg (NegVar var) = PosVar var

-- | Convert a literal to the corresponding logical formula
toFormula :: Literal -> Formula
toFormula _ = undefined -- TODO
