{-# LANGUAGE OverloadedStrings #-}

module Language.While.Property.LowEq where

import Data.Map.Strict qualified as Map

import Prettyprinter (Doc, pretty, (<+>))

import Language.While.Eval
import Language.While.Eval.Env qualified as Env
import Language.While.Secure

checkLowEq :: Level -> Value -> Value -> Bool
checkLowEq High _ _ = True
checkLowEq Low v1 v2 = v1 == v2

checkLowEqEnv :: SecurityMap -> Env -> Env -> Maybe (Doc ())
checkLowEqEnv secMap env1 env2 =
  if all (uncurry go) $ Map.toList $ getSecMap secMap
    then Nothing
    else Just $ pretty env1 <+> "/=" <+> pretty env2
 where
  go x s = do
    let v1 = Env.lookupUnsafe x env1
    let v2 = Env.lookupUnsafe x env2
    checkLowEq s v1 v2
