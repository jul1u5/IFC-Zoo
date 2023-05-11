{-# LANGUAGE DataKinds #-}

module Language.While.Security where

import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map

import Language.While.Abstract.Name
import Language.While.Eval.Env (Env)
import Language.While.Eval.Env qualified as Env
import Language.While.Eval.Value

data Level = Low | High
  deriving (Eq, Show, Ord)

instance Semigroup Level where
  High <> _ = High
  _ <> High = High
  Low <> Low = Low

type SecurityMap = Map Name Level

checkLowEq :: Level -> Value -> Value -> Bool
checkLowEq High _ _ = True
checkLowEq Low v1 v2 = v1 == v2

checkLowEqEnv :: SecurityMap -> Env -> Env -> Bool
checkLowEqEnv secMap env1 env2 = all (uncurry go) $ Map.toList secMap
 where
  go x s = do
    let v1 = Env.lookup x env1
    let v2 = Env.lookup x env2
    checkLowEq s v1 v2
