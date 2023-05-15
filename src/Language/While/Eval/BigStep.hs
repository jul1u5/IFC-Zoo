{-# LANGUAGE TypeFamilies #-}

module Language.While.Eval.BigStep where

import Control.Monad.State.Strict

import Language.While.Eval.Env (Env, SValue (..))
import Language.While.Eval.Env qualified as Env
import Language.While.Eval.Expr qualified as Expr
import Language.While.Typed

type M = State Env

newtype Eval = Eval {evalCmd :: M ()}

eval :: Eval -> Env
eval = evalIn Env.empty

evalIn :: Env -> Eval -> Env
evalIn env = flip execState env . evalCmd

evalExpr :: Expr.Eval t -> M (SValue t)
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
        SVBool True -> evalCmd c1
        SVBool False -> evalCmd c2

  while_ e c =
    Eval $
      evalExpr e >>= \case
        SVBool True -> do
          evalCmd c
          evalCmd $ while_ e c
        SVBool False -> return ()

  ass_ x e = Eval $ do
    v <- evalExpr e
    modify' $ Env.update x v
