{-# LANGUAGE TypeFamilies #-}

module Language.While.Eval.BigStep where

import Data.Map qualified as Map

import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State.Strict

import Language.While.Abstract
import Language.While.Eval.Context
import Language.While.Eval.Expr qualified as Expr

newtype Eval = Eval {eval :: StateT Env (Except EvalError) ()}

runEval :: Eval -> Either EvalError Env
runEval = runExcept . flip execStateT (Env Map.empty) . eval

type instance WhileExpr Eval = Expr.Eval

evalExpr :: Expr.Eval -> StateT Env (Except EvalError) Value
evalExpr e = do
  env <- get
  lift $ runReaderT (Expr.eval e) env

instance While Eval where
  skip_ = Eval $ pure ()

  semicolon s1 s2 = Eval $ do
    eval s1
    eval s2

  if_ cond (Then s1) (Else s2) =
    Eval $
      evalExpr cond >>= \case
        VInt _ -> throwError $ TypeMismatch{expected = TBool, actual = TInt}
        VBool True -> eval s1
        VBool False -> eval s2

  while_ cond s =
    Eval $
      evalExpr cond >>= \case
        VInt _ -> throwError $ TypeMismatch{expected = TBool, actual = TInt}
        VBool False -> pure ()
        VBool True -> do
          eval s
          eval $ while_ cond s

  n .= e = Eval $ do
    v <- evalExpr e
    modify' $ Env . Map.insert n v . vars
