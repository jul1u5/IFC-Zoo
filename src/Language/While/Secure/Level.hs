{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

module Language.While.Secure.Level where

import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map

import Prettyprinter (Pretty, pretty)

import Language.While.Abstract qualified as A
import Language.While.Eval.Env (Env)
import Language.While.Eval.Env qualified as Env
import Language.While.Eval.Value

data Level = Low | High
  deriving (Eq, Ord)

instance Pretty Level where
  pretty = \case
    Low -> "low"
    High -> "high"

instance Semigroup Level where
  High <> _ = High
  _ <> High = High
  Low <> Low = Low

type SecurityMap = Map A.Name Level

checkLowEq :: Level -> Value -> Value -> Bool
checkLowEq High _ _ = True
checkLowEq Low v1 v2 = v1 == v2

checkLowEqEnv :: SecurityMap -> Env -> Env -> Bool
checkLowEqEnv secMap env1 env2 = all (uncurry go) $ Map.toList secMap
 where
  go x s = do
    let v1 = Env.lookupUnsafe x env1
    let v2 = Env.lookupUnsafe x env2
    checkLowEq s v1 v2
