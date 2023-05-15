{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

module Language.While.Eval.Type where

import Prettyprinter (Pretty, pretty)

data Type = TInt | TBool
  deriving (Eq, Show)

data SType (t :: Type) where
  SInt :: SType 'TInt
  SBool :: SType 'TBool

instance Pretty Type where
  pretty = \case
    TInt -> "int"
    TBool -> "bool"

data SomeSType where
  SomeSType :: SType t -> SomeSType

toSType :: Type -> SomeSType
toSType = \case
  TInt -> SomeSType SInt
  TBool -> SomeSType SBool
