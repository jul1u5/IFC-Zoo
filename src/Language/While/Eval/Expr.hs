module Language.While.Eval.Expr where

import Control.Monad.Except
import Control.Monad.Reader

import Data.Map qualified as Map

import Language.While.Abstract.Expr
import Language.While.Eval.Context

newtype Eval = Eval {eval :: ReaderT Env (Except EvalError) Value}

instance Expr Eval where
  bool_ = Eval . return . VBool
  int_ = Eval . return . VInt

  var_ n = Eval $ do
    env <- asks vars
    case Map.lookup n env of
      Nothing -> throwError $ UndefinedVariable n
      Just v -> return v

  not_ e = Eval $ do
    v <- eval e
    case v of
      VInt _ -> throwError $ TypeMismatch{expected = TBool, actual = TInt}
      VBool b -> return $ VBool $ not b

  (+.) = intOp (+)
  (-.) = intOp (-)
  (*.) = intOp (*)

  (==.) = ordOp (==)
  (<.) = ordOp (<)
  (>.) = ordOp (>)

intOp :: (Int -> Int -> Int) -> Eval -> Eval -> Eval
intOp op e1 e2 = Eval $ do
  v1 <- eval e1 >>= assertInt
  v2 <- eval e2 >>= assertInt
  return $ VInt $ op v1 v2
 where
  assertInt = \case
    VInt i -> return i
    VBool _ -> throwError $ TypeMismatch{expected = TInt, actual = TBool}

ordOp :: (forall a. Ord a => a -> a -> Bool) -> Eval -> Eval -> Eval
ordOp op e1 e2 = Eval $ do
  v1 <- eval e1
  v2 <- eval e2
  case (v1, v2) of
    (VInt _, VBool _) -> throwError $ TypeMismatch{expected = TInt, actual = TBool}
    (VBool _, VInt _) -> throwError $ TypeMismatch{expected = TBool, actual = TInt}
    (VInt x, VInt y) -> return $ VBool $ op x y
    (VBool x, VBool y) -> return $ VBool $ op x y
