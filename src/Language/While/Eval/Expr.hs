{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MonoLocalBinds #-}

module Language.While.Eval.Expr where

import Control.Monad.Reader

import Language.While.Eval.Env
import Language.While.Eval.Env qualified as Env
import Language.While.Eval.Type
import Language.While.Typed.Expr

newtype Eval t = Eval {evalExpr :: Reader Env (SValue t)}

eval :: Env -> Eval t -> SValue t
eval env = flip runReader env . evalExpr

instance Expr Eval where
  bool_ = Eval . return . SVBool
  int_ = Eval . return . SVInt

  var_ n = Eval $ do
    env <- ask
    let v = Env.lookup n env
    return v

  not_ e = Eval $ do
    evalExpr e >>= \case
      SVBool b -> return $ SVBool $ not b

  arithOp = \case
    Plus -> intOp (+)
    Minus -> intOp (-)
    Times -> intOp (*)

  relOp = \case
    Equal -> ordOp (==)
    LessThan -> ordOp (<)
    GreaterThan -> ordOp (>)

intOp :: (Int -> Int -> Int) -> Eval 'TInt -> Eval 'TInt -> Eval 'TInt
intOp op e1 e2 = Eval $ do
  v1 <- evalExpr e1
  v2 <- evalExpr e2
  case (v1, v2) of
    (SVInt i1, SVInt i2) -> return $ SVInt $ op i1 i2

ordOp :: (forall a. Ord a => a -> a -> Bool) -> Eval t -> Eval t -> Eval 'TBool
ordOp op e1 e2 = Eval $ do
  v1 <- evalExpr e1
  v2 <- evalExpr e2
  case (v1, v2) of
    (SVInt i1, SVInt i2) -> do
      return $ SVBool $ op i1 i2
    (SVBool b1, SVBool b2) -> do
      return $ SVBool $ op b1 b2
