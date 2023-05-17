{-# LANGUAGE TypeFamilies #-}

module Language.While.Eval.SmallStep where

import Control.Arrow ((>>>))
import Control.Monad.State.Strict

import Data.Functor ((<&>))

import Language.While.Eval.Env
import Language.While.Eval.Env qualified as Env
import Language.While.Eval.Expr qualified as Expr
import Language.While.Typed

data EvalTransition
  = Term
  | NonTerm Eval

class EvalTransitionC repr where
  term :: repr
  nonTerm :: cont -> repr

type M = State Env

newtype Eval = Eval {evalCmd :: M EvalTransition}

eval :: Eval -> Env
eval = evalIn Env.empty

evalIn :: Env -> Eval -> Env
evalIn env =
  evalCmd
    >>> flip runState env
    >>> \case
      (Term, env') -> env'
      (NonTerm c, env') -> evalIn env' c

evalExpr :: Expr.Eval t -> M (SValue t)
evalExpr e = do
  env <- get
  return $ Expr.eval env e

instance While Eval where
  type WhileExpr Eval = Expr.Eval

  skip_ = Eval $ return Term

  semicolon c1 c2 =
    Eval $
      evalCmd c1 <&> \case
        NonTerm c1' -> NonTerm $ c1' `semicolon` c2
        Term -> NonTerm c2

  if_ e c1 c2 =
    Eval $
      evalExpr e <&> \case
        SVBool True -> NonTerm c1
        SVBool False -> NonTerm c2

  while_ e c =
    Eval $
      evalExpr e <&> \case
        SVBool True -> NonTerm $ c `semicolon` while_ e c
        SVBool False -> Term

  ass_ x e = Eval $ do
    v <- evalExpr e
    modify' $ Env.update x v
    return Term
