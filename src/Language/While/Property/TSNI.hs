{-# LANGUAGE TypeFamilies #-}

module Language.While.Property.TSNI where

import Language.While.Eval.Env
import Language.While.Property.Divergence
import Language.While.Secure.Level
import Language.While.Typed

tsni ::
  While c =>
  (Env -> c -> Env) ->
  c ->
  SecurityMap ->
  Env ->
  Env ->
  Bool
tsni evalIn c secMap env1 env2 = do
  let env1' = runOrDiverge evalIn env1 c
  let env2' = runOrDiverge evalIn env2 c
  checkTS secMap env1' env2'

checkTS :: SecurityMap -> Maybe Env -> Maybe Env -> Bool
checkTS secMap = \cases
  -- both converge
  (Just env1) (Just env2) -> checkLowEqEnv secMap env1 env2
  -- both diverge
  Nothing Nothing -> True
  _ _ -> False
