{-# LANGUAGE TypeFamilies #-}

module Language.While.Eval.SmallStep where

import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State.Strict

import Data.Functor ((<&>))
import Data.Map.Strict qualified as Map

import Language.While.Abstract.Command
import Language.While.Eval.Context
import Language.While.Eval.Expr qualified as Expr

data EvalTransition
  = Term
  | NonTerm Eval

type M = StateT Env (Except EvalError)

newtype Eval = Eval {eval :: M EvalTransition}

runEval :: Eval -> Either EvalError Env
runEval = runEvalWithEnv (Env Map.empty)

runEvalWithEnv :: Env -> Eval -> Either EvalError Env
runEvalWithEnv env =
  runExcept
    . flip runStateT env
    . eval
    >=> \case
      (Term, env') -> return env'
      (NonTerm c, env') -> runEvalWithEnv env' c

evalExpr :: Expr.Eval -> M Value
evalExpr e = do
  env <- get
  lift $ runReaderT (Expr.eval e) env

type instance WhileExpr Eval = Expr.Eval

instance While Eval where
  skip_ = Eval $ return Term
  semicolon c1 c2 =
    Eval $ do
      eval c1
        <&> \case
          NonTerm c1' -> NonTerm $ c1' `semicolon` c2
          Term -> NonTerm c2

  if_ cond (Then c1) (Else c2) = Eval $ do
    evalExpr cond >>= \case
      VInt _ -> throwError $ TypeMismatch{expected = TBool, actual = TInt}
      VBool True -> return $ NonTerm c1
      VBool False -> return $ NonTerm c2

  while_ cond c = Eval $ do
    evalExpr cond >>= \case
      VInt _ -> throwError $ TypeMismatch{expected = TBool, actual = TInt}
      VBool True -> return $ NonTerm $ c `semicolon` while_ cond c
      VBool False -> return Term

  n .= e = Eval $ do
    v <- evalExpr e
    modify' $ Env . Map.insert n v . vars
    return Term
