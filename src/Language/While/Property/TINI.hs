{-# LANGUAGE TypeFamilies #-}

module Language.While.Property.TINI where

import Data.Maybe (fromMaybe)

import Language.While.Eval.Env
import Language.While.Property.Divergence
import Language.While.Secure.Level
import Language.While.Typed

tini ::
  While c =>
  (Env -> c -> Env) ->
  c ->
  SecurityMap ->
  Env ->
  Env ->
  Bool
tini evalIn c secMap env1 env2 = do
  let env1' = runOrDiverge evalIn env1 c
  let env2' = runOrDiverge evalIn env2 c
  checkTI secMap env1' env2'

checkTI :: SecurityMap -> Maybe Env -> Maybe Env -> Bool
checkTI secMap env1 env2 =
  fromMaybe True $
    checkLowEqEnv secMap <$> env1 <*> env2
