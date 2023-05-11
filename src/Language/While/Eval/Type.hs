{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

module Language.While.Eval.Type where

import Prettyprinter (Pretty, pretty, (<+>))

import Control.Monad.Except
import Language.While.Abstract.Name
import Language.While.Eval.Value

data Type = TInt | TBool
  deriving (Eq, Show)

data SType (t :: Type) where
  SInt :: SType 'TInt
  SBool :: SType 'TBool

instance Pretty Type where
  pretty = \case
    TInt -> "int"
    TBool -> "bool"

-- data EvalError
--   = TypeMismatch {expected :: Type, actual :: Type}
--   | UndefinedVariable Name
--   deriving (Eq)

-- expectInt :: MonadError EvalError m => Value -> m Int
-- expectInt = \case
--   VInt i -> return i
--   VBool _ -> throwError $ TypeMismatch{expected = TInt, actual = TBool}

-- expectBool :: MonadError EvalError m => Value -> m Bool
-- expectBool = \case
--   VBool b -> return b
--   VInt _ -> throwError $ TypeMismatch{expected = TBool, actual = TInt}
