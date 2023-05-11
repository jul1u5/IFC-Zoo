{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}

module Language.While.Typed.TypeChecker where

import Control.Monad.Except
import Control.Monad.State.Strict
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Type.Equality ((:~:) (..))
import Language.While.Abstract qualified as A
import Language.While.Eval.Type
import Language.While.Typed qualified as T
import Language.While.Typed.Eq
import Prettyprinter (Pretty, pretty, (<+>))
import Prettyprinter qualified as P

newtype Context = Context {context :: Map A.Name Type}

instance Pretty Context where
  pretty Context{context} =
    P.vsep
      . map (\(key, value) -> pretty key <+> "|->" <+> pretty value)
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

-- type instance T.WhileExpr (TypeCheck e) =

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
      Just (toS -> SomeSType t') -> do
        Refl <- t =?= t'
        return $ T.ass_ (T.Name x) e'

instance T.Expr e => A.Expr (TypeCheckExpr e) where
  bool_ b = TypeCheckExpr $ return $ T.bool_ b ::: SBool
  int_ i = TypeCheckExpr $ return $ T.int_ i ::: SInt
  var_ x = TypeCheckExpr $ do
    ctx <- get
    case Map.lookup x (context ctx) of
      Nothing -> throwError $ UndefinedVariable x
      Just (toS -> SomeSType t) -> return $ T.var_ (T.Name x) ::: t

  not_ e = TypeCheckExpr $ do
    e' ::: t <- typeCheckExpr e
    case t of
      SInt -> throwError $ TypeMismatch{expected = TBool, actual = TInt}
      SBool -> return $ T.not_ e' ::: SBool

  e1 ==. e2 = TypeCheckExpr $ do
    e1' ::: t1 <- typeCheckExpr e1
    e2' ::: t2 <- typeCheckExpr e2
    Refl <- t1 =?= t2
    return $ T.relOp T.Equal e1' e2' ::: SBool

data SomeSType where
  SomeSType :: SType t -> SomeSType

toS :: Type -> SomeSType
toS = \case
  TInt -> SomeSType SInt
  TBool -> SomeSType SBool

expectInt :: MonadError TypeCheckError m => Type -> m ()
expectInt = \case
  TInt -> return ()
  TBool -> throwError $ TypeMismatch{expected = TInt, actual = TBool}

expectBool :: MonadError TypeCheckError m => Type -> m ()
expectBool = \case
  TBool -> return ()
  TInt -> throwError $ TypeMismatch{expected = TBool, actual = TInt}
