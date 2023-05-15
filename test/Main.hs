{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module Main where

import Control.Monad (unless)

import Data.Map.Strict qualified as Map

import Test.Falsify.Generator qualified as Gen
import Test.Falsify.Range qualified as Range
import Test.Tasty
import Test.Tasty.Falsify

import Language.While.Abstract qualified as A
import Language.While.Eval
import Language.While.Eval.BigStep qualified as BigStep
import Language.While.Eval.BigStep qualified as SmallStep
import Language.While.Examples
import Language.While.Pretty
import Language.While.Property
import Language.While.Secure
import Language.While.Typed qualified as T

main :: IO ()
main = do
  defaultMain $
    testGroup
      "IFC-Zoo"
      [ testGroup
          "TINI"
          [ testProperty "explicit flow" $ typeCheckAndRun prop_TINI explicitFlow
          , testProperty "implicit flow" $ typeCheckAndRun prop_TINI implicitFlow
          ]
      , testGroup
          "TSNI"
          [ testProperty "non-terminating" $ typeCheckAndRun prop_TSNI nonTerminating
          ]
      ]

typeCheckAndRun ::
  T.While c =>
  (c -> SecurityMap -> Env -> Env -> Property ()) ->
  (forall a. A.While a => a) ->
  Property ()
typeCheckAndRun cont prog = do
  let secMap = Map.fromList [("l", Low), ("h", High)]
  (ctx, env1, env2) <- gen' genEnv

  case T.typeCheckIn ctx prog of
    Left err -> testFailed $ showPretty err
    Right (typedProg, typedProg') -> do
      case checkFlow secMap typedProg of
        Left _err ->
          return ()
        Right () ->
          cont typedProg' secMap env1 env2

prop_TINI :: BigStep.Eval -> SecurityMap -> Env -> Env -> Property ()
prop_TINI prog secMap env1 env2 = do
  let succeeded = tini BigStep.evalIn prog secMap env1 env2
  unless succeeded $ testFailed "TINI violation"

prop_TSNI :: SmallStep.Eval -> SecurityMap -> Env -> Env -> Property ()
prop_TSNI prog secMap env1 env2 = do
  let succeeded = tsni SmallStep.evalIn prog secMap env1 env2
  unless succeeded $ testFailed "TSNI violation"

gen' :: Pretty a => Gen a -> Property' e a
gen' = genWith (Just . showPretty)

genEnv :: Gen (T.Context, Env, Env)
genEnv = do
  t_l <- genType
  v_l <- genValue t_l
  t_h <- genType
  v_h1 <- genValue t_h
  v_h2 <- genValue t_h
  let lowEnv = Env $ Map.fromList [("l", v_l)]
  let highEnv1 = Env $ Map.fromList [("h", v_h1)]
  let highEnv2 = Env $ Map.fromList [("h", v_h2)]
  let ctx = T.Context $ Map.fromList [("l", t_l), ("h", t_h)]
  return (ctx, lowEnv <> highEnv1, lowEnv <> highEnv2)

genType :: Gen Type
genType = pure TBool -- Gen.choose (pure TBool) (pure TInt)

genValue :: Type -> Gen Value
genValue = \case
  TBool -> VBool <$> Gen.bool False
  TInt -> VInt <$> Gen.int (Range.between (-100, 100))
