{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}

module Language.While.Secure.Flow where

import Control.Monad.Except
import Control.Monad.Reader

import Data.Map.Strict qualified as Map
import Data.Maybe (fromJust)

import GHC.Stack (HasCallStack)

import Prettyprinter (Pretty (pretty), (<+>))
import Prettyprinter qualified as P

import Language.While.Abstract qualified as A
import Language.While.Secure.Level
import Language.While.Typed

data SecError = AssignmentOfHighToLow
  { varName :: A.Name
  , varLevel :: Level
  , expLevel :: Level
  }

instance Pretty SecError where
  pretty = \case
    AssignmentOfHighToLow{..} ->
      "Flow from"
        <+> pretty expLevel
        <+> "expression"
        <+> "to"
        <+> pretty varLevel
        <+> "variable"
        <+> P.parens (pretty varName)
        <+> "violates security policy"

data SecContext = SecContext
  { secMap :: SecurityMap
  , secLevel :: Level
  }

type M = ReaderT SecContext (Except SecError)

newtype SecFlow = SecFlow
  { secCheck :: M ()
  }

checkFlow :: SecurityMap -> SecFlow -> Either SecError ()
checkFlow secMap x = do
  runExcept $ runReaderT (secCheck x) ctx
 where
  ctx = SecContext secMap Low

type MExpr = ReaderT SecurityMap (Except SecError)

newtype SecFlowExpr t = SecFlowExpr
  { secCheckExpr :: MExpr Level
  }

type instance WhileExpr SecFlow = SecFlowExpr

checkExpr :: WhileExpr SecFlow t -> M Level
checkExpr e = do
  secMap <- asks secMap
  lift $ runReaderT (secCheckExpr e) secMap

instance While SecFlow where
  skip_ = SecFlow $ return ()

  semicolon c1 c2 = SecFlow $ secCheck c1 >> secCheck c2

  if_ e c1 c2 = SecFlow $ do
    eLevel <- checkExpr e
    local (updateLevel eLevel) $ do
      secCheck c1
      secCheck c2

  while_ e c = SecFlow $ do
    eLevel <- checkExpr e
    local (updateLevel eLevel) $ do
      secCheck c

  ass_ x e = SecFlow $ do
    xLevel <- lookupLevel' x
    eLevel <- checkExpr e
    pLevel <- asks secLevel
    unless (eLevel <> pLevel <= xLevel) $ do
      throwError $
        AssignmentOfHighToLow
          { varName = untypedName x
          , varLevel = xLevel
          , expLevel = eLevel
          }

lookupLevel ::
  ( HasCallStack
  , MonadReader SecurityMap m
  , MonadError SecError m
  ) =>
  Name t ->
  m Level
lookupLevel (Name x) = asks $ fromJust . Map.lookup x

lookupLevel' ::
  ( HasCallStack
  , MonadError SecError m
  ) =>
  Name t ->
  ReaderT SecContext m Level
lookupLevel' x = withReaderT secMap $ lookupLevel x

updateLevel :: Level -> SecContext -> SecContext
updateLevel l ctx = ctx{secLevel = l <> secLevel ctx}

instance Expr SecFlowExpr where
  bool_ _ = SecFlowExpr $ return Low
  int_ _ = SecFlowExpr $ return Low
  var_ x = SecFlowExpr $ lookupLevel x
  not_ e = e
  arithOp _ (SecFlowExpr e1) (SecFlowExpr e2) = SecFlowExpr $ (<>) <$> e1 <*> e2
  relOp _ (SecFlowExpr e1) (SecFlowExpr e2) = SecFlowExpr $ (<>) <$> e1 <*> e2
