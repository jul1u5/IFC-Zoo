module Language.While.Abstract.Expr where

import Language.While.Abstract.Name

class Expr e where
  bool_ :: Bool -> e
  int_ :: Int -> e

  var_ :: Name -> e

  not_ :: e -> e

  (+.) :: e -> e -> e
  (-.) :: e -> e -> e
  (*.) :: e -> e -> e

  (==.) :: e -> e -> e
  (<.) :: e -> e -> e
  (>.) :: e -> e -> e

infix 4 ==.
infix 4 <.
infix 4 >.
