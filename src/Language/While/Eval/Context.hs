{-# LANGUAGE OverloadedStrings #-}

module Language.While.Eval.Context where

import Data.Bool (bool)
import Data.Map qualified as Map
import Data.Map.Strict (Map)

import Prettyprinter (Pretty, pretty, (<+>))
import Prettyprinter qualified as P

import Language.While.Abstract.Name

data Value
  = VBool Bool
  | VInt Int

instance Pretty Value where
  pretty = \case
    VBool b -> bool "false" "true" b
    VInt i -> pretty i

newtype Env = Env {vars :: Map Name Value}

instance Pretty Env where
  pretty Env{vars} =
    P.vsep
      . map (\(key, value) -> pretty key <+> "|->" <+> pretty value)
      . Map.toList
      $ vars

data Type = TInt | TBool

instance Pretty Type where
  pretty = \case
    TInt -> "int"
    TBool -> "bool"

data EvalError
  = TypeMismatch {expected :: Type, actual :: Type}
  | UndefinedVariable Name

instance Pretty EvalError where
  pretty = \case
    TypeMismatch{expected, actual} ->
      "Type mismatch:"
        <+> "expected"
        <+> pretty expected
        <+> "but got"
        <+> pretty actual
    UndefinedVariable name ->
      "Undefined variable:" <+> pretty name
