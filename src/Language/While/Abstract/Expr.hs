module Language.While.Abstract.Expr where

import Language.While.Abstract.Name

class Expr e where
  bool_ :: Bool -> e
  int_ :: Int -> e

  var_ :: Name -> e

  not_ :: Expr e => e -> e

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

  {-# MINIMAL bool_, int_, var_, not_, (infix_ | (+.), (-.), (*.), (==.), (<.), (>.)) #-}

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
