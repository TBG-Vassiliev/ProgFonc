{-# LANGUAGE GADTs, KindSignatures, DeriveGeneric #-}

module Literal
  ( Literal(..),
    neg,
    toFormula
  ) where

import Data.Kind (Type)
import GHC.Generics (Generic)
import Formula (Formula(..))  -- Supposons que Formula a des constructeurs comme Var et Not

data Literal :: Type where
  Positive :: String -> Literal
  Negative :: String -> Literal
  deriving (Eq, Ord, Show, Generic)

neg :: Literal -> Literal
neg (Positive s) = Negative s
neg (Negative s) = Positive s

toFormula :: Literal -> Formula
toFormula (Positive s) = Var s          -- Supposons que Var est un constructeur de Formula
toFormula (Negative s) = Not (Var s)    -- Supposons que Not est un constructeur de Formula
