{-# LANGUAGE OverloadedStrings #-}

module Language.While.Typed.TypeEquality where

import Control.Monad.Except

import Data.Type.Equality ((:~:) (..))

import Prettyprinter

import Language.While.Abstract qualified as A
import Language.While.Eval.Type

data TypeCheckError
  = TypeMismatch {expected :: Type, actual :: Type}
  | UndefinedVariable A.Name
  deriving (Eq, Show)

instance Pretty TypeCheckError where
  pretty = \case
    TypeMismatch{expected, actual} ->
      "Type mismatch:"
        <+> "expected"
        <+> pretty expected
        <+> "but got"
        <+> pretty actual
    UndefinedVariable name ->
      "Undefined variable:" <+> pretty name

(=?=) :: MonadError TypeCheckError m => SType t1 -> SType t2 -> m (t1 :~: t2)
SInt =?= SInt = return Refl
SBool =?= SBool = return Refl
SInt =?= SBool = throwError $ TypeMismatch{expected = TInt, actual = TBool}
SBool =?= SInt = throwError $ TypeMismatch{expected = TBool, actual = TInt}
