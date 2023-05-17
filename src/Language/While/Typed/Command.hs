{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE TypeFamilies #-}

module Language.While.Typed.Command where

import Data.Kind qualified as Kind

import Language.While.Eval.Type
import Language.While.Typed.Expr

class (Expr (WhileExpr c)) => While c where
  type WhileExpr c :: Type -> Kind.Type
  skip_ :: c
  semicolon :: c -> c -> c
  if_ :: WhileExpr c 'TBool -> c -> c -> c
  while_ :: WhileExpr c 'TBool -> c -> c
  ass_ :: Name t -> WhileExpr c t -> c

instance (While c, While c') => While (c, c') where
  type WhileExpr (c, c') = TypedPair (WhileExpr c) (WhileExpr c')

  skip_ = (skip_, skip_)
  semicolon (c1, c1') (c2, c2') = (c1 `semicolon` c2, c1' `semicolon` c2')
  if_ (e :*: e') (c1, c1') (c2, c2') = (if_ e c1 c2, if_ e' c1' c2')
  while_ (e :*: e') (c, c') = (while_ e c, while_ e' c')
  ass_ x (e :*: e') = (ass_ x e, ass_ x e')
