{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

module Language.While.Secure.Level where

import Data.Map.Strict (Map)

import Prettyprinter (Pretty, pretty)

import Language.While.Abstract qualified as A

data Level = Low | High
  deriving (Eq, Ord)

instance Pretty Level where
  pretty = \case
    Low -> "low"
    High -> "high"

instance Semigroup Level where
  High <> _ = High
  _ <> High = High
  Low <> Low = Low

newtype SecurityMap = SecMap {getSecMap :: Map A.Name Level}
