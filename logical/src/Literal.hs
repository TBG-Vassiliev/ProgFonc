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
data Literal :: Type -- TODO
--  deriving (Eq, Ord)
-- !!! Implementation MUST derive typeclasses 'Eq' and 'Ord' so the previous line of code must be uncommented

instance Show Literal where
  show _ = undefined

-- | Convert boolean value to constant literal
fromBool :: Bool -> Literal
fromBool _ = undefined -- TODO

-- | Convert logical variable to a positive literal
fromPositive :: String -> Literal
fromPositive _ = undefined -- TODO

-- | Convert logical variable to a positive literal
fromNegative :: String -> Literal
fromNegative _ = undefined -- TODO

-- | Negation operation
neg :: Literal -> Literal
neg _ = undefined -- TODO

-- | Convert a literal to the corresponding logical formula
toFormula :: Literal -> Formula
toFormula _ = undefined -- TODO
