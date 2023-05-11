{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

module Language.While.Check.TSNI where

import Data.Maybe (fromMaybe)

import System.IO.Unsafe (unsafePerformIO)
import System.Timeout (timeout)

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

tsni ::
  While c =>
  (Env -> c -> Env) ->
  c ->
  SecurityMap ->
  Env ->
  Env ->
  Bool
tsni evalIn c secMap env1 env2 = do
  let env1' = checkDivergence $ evalIn env1 c
  let env2' = checkDivergence $ evalIn env2 c
  checkTS secMap env1' env2'
 where
  checkDivergence x = unsafePerformIO $ timeout microseconds $ pure $! x
  microseconds = 1

checkTS :: SecurityMap -> Maybe Env -> Maybe Env -> Bool
checkTS secMap = \cases
  (Just env1) (Just env2) -> checkLowEqEnv secMap env1 env2
  Nothing Nothing -> True
  _ _ -> False
