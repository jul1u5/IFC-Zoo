{-# LANGUAGE OverloadedStrings #-}

module Language.While.Eval.Env where

import Data.Map qualified as Map
import Data.Map.Strict (Map)

import Prettyprinter (Pretty, pretty, (<+>))
import Prettyprinter qualified as P

import Language.While.Abstract.Name
import Language.While.Eval.Value

newtype Env = Env {vars :: Map Name Value}
  deriving (Eq, Show, Semigroup, Monoid)

empty :: Env
empty = mempty

lookup :: Name -> Env -> Value
lookup x env = case Map.lookup x $ vars env of
  Nothing -> error $ "undefined variable: " <> show x
  Just v -> v

update :: Name -> Value -> Env -> Env
update x v = Env . Map.insert x v . vars

instance Pretty Env where
  pretty Env{vars} =
    P.vsep
      . map (\(key, value) -> pretty key <+> "|->" <+> pretty value)
      . Map.toList
      $ vars
