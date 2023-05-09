{-# LANGUAGE TypeFamilies #-}

module Language.WhileIO.Eval.SmallStep where

import Control.Monad.Except
import Control.Monad.Reader
import Data.Functor ((<&>))
import Data.Map.Strict qualified as Map
import Language.While.Eval.Context
import Language.While.Eval.Expr qualified as Expr
import Language.WhileIO.Abstract

data EvalTransition a
  = Term Env
  | NonTerm (a, Env)
  deriving (Functor)

newtype Eval a = Eval {eval :: Env -> Except EvalError (EvalTransition (Eval a))}
  deriving (Functor)

type instance WhileExpr (Eval a) = Expr.Eval

evalExpr :: Env -> Expr.Eval -> Except EvalError Value
evalExpr env = flip runReaderT env . Expr.eval

instance While a => While (Eval a) where
  skip_ = Eval $ return . Term
  semicolon c1 s2 =
    Eval $ \env1 ->
      eval c1 env1
        <&> \case
          NonTerm (c1', env2) -> NonTerm (semicolon c1' s2, env2)
          Term env2 -> NonTerm (s2, env2)

  if_ cond (Then c1) (Else c2) = Eval $ \env ->
    evalExpr env cond >>= \case
      VInt _ -> throwError $ TypeMismatch{expected = TBool, actual = TInt}
      VBool True -> return $ NonTerm (c1, env)
      VBool False -> return $ NonTerm (c2, env)

  while_ cond c = Eval $ \env ->
    evalExpr env cond >>= \case
      VInt _ -> throwError $ TypeMismatch{expected = TBool, actual = TInt}
      VBool True -> return $ NonTerm (c, env)
      VBool False -> return $ Term env

  n .= e = Eval $ \env -> do
    v <- evalExpr env e
    return $ Term $ Env $ Map.insert n v $ vars env

-- instance WhileIO a => WhileIO (Eval a) where
--   in_ l n = _
--   out_ l n e = _
