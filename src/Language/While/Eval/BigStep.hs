{-# LANGUAGE TypeFamilies #-}

module Language.While.Eval.BigStep where

import Control.Monad.State.Strict

import Language.While.Eval.Env (Env)
import Language.While.Eval.Env qualified as Env
import Language.While.Eval.Expr qualified as Expr
import Language.While.Eval.Value
import Language.While.Typed

type M = State Env

newtype Eval = Eval {evalCmd :: M ()}

eval :: Eval -> Env
eval = evalIn Env.empty

evalIn :: Env -> Eval -> Env
evalIn env = flip execState env . evalCmd

evalExpr :: Expr.Eval t -> M (Expr.SValue t)
evalExpr e = do
  env <- get
  return $ Expr.eval env e

type instance WhileExpr Eval = Expr.Eval

instance While Eval where
  skip_ = Eval $ return ()

  semicolon c1 c2 = Eval $ do
    evalCmd c1
    evalCmd c2

  if_ e c1 c2 =
    Eval $
      evalExpr e >>= \case
        Expr.SVBool True -> evalCmd c1
        Expr.SVBool False -> evalCmd c2

  while_ e c =
    Eval $
      evalExpr e >>= \case
        Expr.SVBool True -> do
          evalCmd c
          evalCmd $ while_ e c
        Expr.SVBool False -> return ()

  ass_ (Name x) e = Eval $ do
    v <- fromS <$> evalExpr e
    modify' $ Env.update x v

fromS :: Expr.SValue t -> Value
fromS = \case
  Expr.SVBool b -> VBool b
  Expr.SVInt i -> VInt i
