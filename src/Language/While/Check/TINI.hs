{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

module Language.While.Check.TINI where

import Control.Monad.Except

import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe)

import System.IO.Unsafe (unsafePerformIO)
import System.Timeout (timeout)

import Control.Monad.RWS.Strict (RWST, evalRWST)
import Control.Monad.Reader

-- import Language.While.Abstract

import Language.While.Abstract qualified as A
import Language.While.Eval.Env
import Language.While.Security
import Language.While.Typed
import Prettyprinter

newtype SecError = AssignmentOfHighToLow A.Name

instance Pretty SecError where
  pretty = \case
    AssignmentOfHighToLow name ->
      "Assignment of high variable" <+> pretty name <+> "to low variable"

tini ::
  While c =>
  (Env -> c -> Env) ->
  c ->
  SecurityMap ->
  Env ->
  Env ->
  Bool
tini evalIn c secMap env1 env2 = do
  let env1' = checkDivergence $ evalIn env1 c
  let env2' = checkDivergence $ evalIn env2 c
  checkTI secMap env1' env2'
 where
  checkDivergence = unsafePerformIO . timeout microseconds . pure
  microseconds = 100_000

checkTI :: SecurityMap -> Maybe Env -> Maybe Env -> Bool
checkTI secMap env1 env2 = fromMaybe True $ checkLowEqEnv secMap <$> env1 <*> env2

data SecContext = SecContext
  { secMap :: SecurityMap
  , secLevel :: Level
  }

updateLevel :: Level -> SecContext -> SecContext
updateLevel l ctx = ctx{secLevel = l <> secLevel ctx}

type M = RWST SecContext Env () (Except SecError)

newtype SecCheckerino = SecCheckerino
  { secCheck :: M ()
  }

checkSecurity :: SecurityMap -> SecCheckerino -> Either SecError ()
checkSecurity secMap x = do
  runExcept . fmap fst $ evalRWST (secCheck x) ctx ()
 where
  ctx = SecContext secMap Low

type MM = ReaderT SecurityMap (Except SecError)

newtype SecCheckeris t = SecCheckeris
  { secCheckExpr :: MM Level
  }

type instance WhileExpr SecCheckerino = SecCheckeris

checkExpr :: WhileExpr SecCheckerino t -> M Level
checkExpr e = do
  secMap <- asks secMap
  lift $ runReaderT (secCheckExpr e) secMap

instance While SecCheckerino where
  skip_ = SecCheckerino $ return ()

  semicolon c1 c2 = SecCheckerino $ secCheck c1 >> secCheck c2

  if_ e c1 c2 = SecCheckerino $ do
    eLevel <- checkExpr e
    local (updateLevel eLevel) $ do
      secCheck c1
      secCheck c2

  while_ e c = SecCheckerino $ do
    eLevel <- checkExpr e
    local (updateLevel eLevel) $ do
      secCheck c

  ass_ x e = SecCheckerino $ do
    xLevel <- lookupLevel' x
    eLevel <- checkExpr e
    pLevel <- asks secLevel
    unless (eLevel <> pLevel <= xLevel) $ do
      throwError $ AssignmentOfHighToLow $ untypedName x

lookupLevel ::
  ( MonadReader SecurityMap m
  , MonadError SecError m
  ) =>
  Name t ->
  m Level
lookupLevel (Name n) = do
  maybeLevel <- asks $ Map.lookup n
  maybe undefined pure maybeLevel

lookupLevel' ::
  (MonadReader SecContext m, MonadError SecError m) =>
  Name t ->
  m Level
lookupLevel' (Name n) = do
  maybeLevel <- asks $ Map.lookup n . secMap
  maybe undefined pure maybeLevel

instance Expr SecCheckeris where
  bool_ _ = SecCheckeris $ return Low
  int_ _ = SecCheckeris $ return Low
  var_ x = SecCheckeris $ lookupLevel x
  not_ e = e
  arithOp _ (SecCheckeris e1) (SecCheckeris e2) = SecCheckeris $ (<>) <$> e1 <*> e2
  relOp _ (SecCheckeris e1) (SecCheckeris e2) = SecCheckeris $ (<>) <$> e1 <*> e2
