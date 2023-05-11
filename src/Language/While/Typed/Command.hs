{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE TypeFamilies #-}

module Language.While.Typed.Command where

import Data.Kind qualified as Kind
import Language.While.Eval.Type
import Language.While.Typed.Expr

type family WhileExpr c :: Type -> Kind.Type

class (Expr (WhileExpr c)) => While c where
  skip_ :: c
  semicolon :: c -> c -> c
  if_ :: WhileExpr c 'TBool -> c -> c -> c
  while_ :: WhileExpr c 'TBool -> c -> c
  ass_ :: Name t -> WhileExpr c t -> c
