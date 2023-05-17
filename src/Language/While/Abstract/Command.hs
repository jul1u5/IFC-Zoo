{-# LANGUAGE TypeFamilies #-}

module Language.While.Abstract.Command where

import Language.While.Abstract.Expr
import Language.While.Abstract.Name

class (Expr (WhileExpr c)) => While c where
  type WhileExpr c

  skip_ :: c
  semicolon :: c -> c -> c
  if_ :: WhileExpr c -> Then c -> Else c -> c
  while_ :: WhileExpr c -> c -> c
  (.=) :: Name -> WhileExpr c -> c

infix 2 .=

newtype Then c = Then c
newtype Else c = Else c

do_ :: (While c) => [c] -> c
do_ = foldr1 semicolon
