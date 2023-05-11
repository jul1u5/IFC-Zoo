{-# LANGUAGE DataKinds #-}

module Language.While.Typed.Expr where

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
