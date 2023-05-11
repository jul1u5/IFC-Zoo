{-# LANGUAGE OverloadedStrings #-}

module Language.While.Eval.Value where

import Data.Bool (bool)

import Prettyprinter (Pretty, pretty)

data Value
  = VBool Bool
  | VInt Int
  deriving (Eq, Show)

instance Pretty Value where
  pretty = \case
    VBool b -> bool "false" "true" b
    VInt i -> pretty i
