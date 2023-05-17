{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}

module Language.While.Property.TSNI where

import Prettyprinter (Doc, pretty, (<+>))

import Language.While.Eval.Env
import Language.While.Property.Divergence
import Language.While.Property.LowEq
import Language.While.Secure.Level
import Language.While.Typed

tsni ::
  While c =>
  (Env -> c -> Env) ->
  c ->
  SecurityMap ->
  Env ->
  Env ->
  Maybe (Doc ())
tsni evalIn c secMap env1 env2 = do
  let maybeEnv1' = runOrDiverge evalIn env1 c
  let maybeEnv2' = runOrDiverge evalIn env2 c
  case (maybeEnv1', maybeEnv2') of
    (Just env1', Just env2') -> checkLowEqEnv secMap env1' env2'
    -- both diverge
    (Nothing, Nothing) -> Nothing
    (serialize -> env1', serialize -> env2') -> Just $ env1' <+> "/=" <+> env2'
 where
  serialize = maybe "◊" pretty
