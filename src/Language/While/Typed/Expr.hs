{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}

module Language.While.Typed.Expr where

import Data.Kind qualified as Kind

import Language.While.Abstract qualified as A
import Language.While.Eval.Type

newtype Name (t :: Type) = Name {untypedName :: A.Name}

class Expr e where
  bool_ :: Bool -> e 'TBool
  int_ :: Int -> e 'TInt

  var_ :: Name t -> e t

  not_ :: e 'TBool -> e 'TBool

  arithOp :: ArithOp -> e 'TInt -> e 'TInt -> e 'TInt

  relOp :: RelOp -> e t -> e t -> e 'TBool

data PrefixOp
  = Negate

data ArithOp
  = Plus
  | Minus
  | Times

data RelOp
  = Equal
  | LessThan
  | GreaterThan

type family WhileExpr c :: Type -> k

data TypedPair a b t where
  (:*:) :: a t -> b t -> TypedPair a b t

type instance WhileExpr (c, c') = TypedPair (WhileExpr c) (WhileExpr c')

instance (Expr e, Expr e') => Expr (TypedPair e e') where
  bool_ b = bool_ b :*: bool_ b
  int_ i = int_ i :*: int_ i
  var_ x = var_ x :*: var_ x
  not_ (e :*: e') = not_ e :*: not_ e'
  arithOp op (e1 :*: e1') (e2 :*: e2') = arithOp op e1 e2 :*: arithOp op e1' e2'
  relOp op (e1 :*: e1') (e2 :*: e2') = relOp op e1 e2 :*: relOp op e1' e2'
