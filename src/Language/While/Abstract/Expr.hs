module Language.While.Abstract.Expr where

import Language.While.Abstract.Name

class Expr e where
  bool_ :: Bool -> e
  int_ :: Int -> e

  var_ :: Name -> e

  prefix_ :: PrefixOp -> e -> e
  prefix_ = \case
    Negate -> not_

  not_ :: Expr e => e -> e
  not_ = prefix_ Negate

  infix_ :: InfixOp -> e -> e -> e
  infix_ = \case
    Plus -> (+.)
    Minus -> (-.)
    Times -> (*.)
    Equal -> (==.)
    LessThan -> (<.)
    GreaterThan -> (>.)

  (+.), (-.), (*.), (==.), (<.), (>.) :: Expr e => e -> e -> e
  (+.) = infix_ Plus
  (-.) = infix_ Minus
  (*.) = infix_ Times
  (==.) = infix_ Equal
  (<.) = infix_ LessThan
  (>.) = infix_ GreaterThan

data PrefixOp
  = Negate

data InfixOp
  = Plus
  | Minus
  | Times
  | Equal
  | LessThan
  | GreaterThan

infix 4 ==.
infix 4 <.
infix 4 >.
