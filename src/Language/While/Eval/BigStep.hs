{-# LANGUAGE TypeFamilies #-}

module Language.While.Eval.BigStep where

import Data.Map qualified as Map

import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State.Strict

import Language.While.Abstract
import Language.While.Eval.Context
import Language.While.Eval.Expr qualified as Expr

type M = StateT Env (Except EvalError)

newtype Eval = Eval {eval :: M ()}

runEval :: Eval -> Either EvalError Env
runEval = runExcept . flip execStateT (Env Map.empty) . eval

evalExpr :: Expr.Eval -> M Value
evalExpr e = do
  env <- get
  lift $ runReaderT (Expr.eval e) env

type instance WhileExpr Eval = Expr.Eval

instance While Eval where
  skip_ = Eval $ return ()

  semicolon c1 c2 = Eval $ do
    eval c1
    eval c2

  if_ cond (Then c1) (Else c2) =
    Eval $
      evalExpr cond >>= \case
        VInt _ -> throwError $ TypeMismatch{expected = TBool, actual = TInt}
        VBool True -> eval c1
        VBool False -> eval c2

  while_ cond c =
    Eval $
      evalExpr cond >>= \case
        VInt _ -> throwError $ TypeMismatch{expected = TBool, actual = TInt}
        VBool False -> return ()
        VBool True -> do
          eval c
          eval $ while_ cond c

  n .= e = Eval $ do
    v <- evalExpr e
    modify' $ Env . Map.insert n v . vars
