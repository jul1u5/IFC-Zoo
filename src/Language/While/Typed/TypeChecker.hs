{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}

module Language.While.Typed.TypeChecker (
  Context (..),
  typeCheck,
  typeCheckIn,
) where

import Control.Monad.Except
import Control.Monad.State.Strict

import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Type.Equality ((:~:) (..))

import Prettyprinter (Pretty, pretty, (<+>))
import Prettyprinter qualified as P

import Language.While.Abstract qualified as A
import Language.While.Eval.Type
import Language.While.Typed.Command qualified as T
import Language.While.Typed.Expr qualified as T
import Language.While.Typed.TypeEquality

newtype Context = Context {context :: Map A.Name Type}

instance Pretty Context where
  pretty Context{context} =
    P.encloseSep P.lbrace P.rbrace P.comma
      . map (\(key, value) -> pretty key <> ":" <+> pretty value)
      . Map.toList
      $ context

type M = StateT Context (Except TypeCheckError)

newtype TypeCheck c = TypeCheck {typeCheckCmd :: M c}

data Typed e where
  (:::) :: e t -> SType t -> Typed e

newtype TypeCheckExpr e = TypeCheckExpr {typeCheckExpr :: M (Typed e)}

typeCheck :: T.While c => TypeCheck c -> Either TypeCheckError c
typeCheck = typeCheckIn $ Context mempty

typeCheckIn :: T.While c => Context -> TypeCheck c -> Either TypeCheckError c
typeCheckIn ctx =
  runExcept . flip evalStateT ctx . typeCheckCmd

type instance A.WhileExpr (TypeCheck c) = TypeCheckExpr (T.WhileExpr c)

instance T.While c => A.While (TypeCheck c) where
  skip_ = TypeCheck $ return T.skip_

  semicolon c1 c2 = TypeCheck $ do
    T.semicolon <$> typeCheckCmd c1 <*> typeCheckCmd c2

  if_ e (A.Then c1) (A.Else c2) =
    TypeCheck $ do
      e' ::: t <- typeCheckExpr e
      case t of
        SInt -> throwError $ TypeMismatch{expected = TBool, actual = TInt}
        SBool -> do
          c1' <- typeCheckCmd c1
          c2' <- typeCheckCmd c2
          return $ T.if_ e' c1' c2'

  while_ e c =
    TypeCheck $ do
      e' ::: t <- typeCheckExpr e
      case t of
        SInt -> throwError $ TypeMismatch{expected = TBool, actual = TInt}
        SBool -> do
          c' <- typeCheckCmd c
          return $ T.while_ e' c'

  x .= e = TypeCheck $ do
    e' ::: t <- typeCheckExpr e
    ctx <- get
    case Map.lookup x (context ctx) of
      Nothing -> throwError $ UndefinedVariable x
      Just (toSType -> SomeSType t') -> do
        Refl <- t =?= t'
        return $ T.ass_ (T.Name x) e'

instance T.Expr e => A.Expr (TypeCheckExpr e) where
  bool_ b = TypeCheckExpr $ return $ T.bool_ b ::: SBool
  int_ i = TypeCheckExpr $ return $ T.int_ i ::: SInt
  var_ x = TypeCheckExpr $ do
    ctx <- get
    case Map.lookup x (context ctx) of
      Nothing -> throwError $ UndefinedVariable x
      Just (toSType -> SomeSType t) -> return $ T.var_ (T.Name x) ::: t

  not_ e = TypeCheckExpr $ do
    e' ::: t <- typeCheckExpr e
    case t of
      SInt -> throwError $ TypeMismatch{expected = TBool, actual = TInt}
      SBool -> return $ T.not_ e' ::: SBool

  infix_ =
    TypeCheckExpr .:. \case
      A.Plus -> checkArithOp T.Plus
      A.Minus -> checkArithOp T.Minus
      A.Times -> checkArithOp T.Times
      A.Equal -> checkRelOp T.Equal
      A.LessThan -> checkRelOp T.LessThan
      A.GreaterThan -> checkRelOp T.GreaterThan

(.:.) :: (d -> e) -> (a -> b -> c -> d) -> a -> b -> c -> e
(f .:. g) x y z = f $ g x y z

checkArithOp :: T.Expr e => T.ArithOp -> TypeCheckExpr e -> TypeCheckExpr e -> M (Typed e)
checkArithOp op e1 e2 = do
  e1' ::: t <- typeCheckExpr e1
  e2' ::: t' <- typeCheckExpr e2
  case (t, t') of
    (SInt, SInt) -> return $ T.arithOp op e1' e2' ::: SInt
    (_, _) -> throwError $ TypeMismatch{expected = TInt, actual = TBool}

checkRelOp :: T.Expr e => T.RelOp -> TypeCheckExpr e -> TypeCheckExpr e -> M (Typed e)
checkRelOp op e1 e2 = do
  e1' ::: t1 <- typeCheckExpr e1
  e2' ::: t2 <- typeCheckExpr e2
  Refl <- t1 =?= t2
  return $ T.relOp op e1' e2' ::: SBool
