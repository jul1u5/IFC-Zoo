{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module Main where

import Control.Monad (unless)

import Data.Map.Strict qualified as Map

import Test.Falsify.Generator qualified as Gen
import Test.Falsify.Range qualified as Range
import Test.Tasty
import Test.Tasty.Falsify

import Data.Either (fromRight)
import Language.While.Abstract
import Language.While.Check.TINI
import Language.While.Check.TSNI (tsni)
import Language.While.Eval.BigStep qualified as BigStep
import Language.While.Eval.Env (Env (..))
import Language.While.Eval.Type
import Language.While.Eval.Value
import Language.While.Examples
import Language.While.Pretty
import Language.While.Security
import Language.While.Typed.TypeChecker (Context (..), typeCheckIn)

main :: IO ()
main = do
  defaultMain $
    testGroup
      "IFC-Zoo"
      [ testGroup
          "TINI"
          [ testProperty "explicit flow" $ prop_TINI explicitFlow
          , testProperty "implicit flow" $ prop_TINI implicitFlow
          ]
      , testGroup
          "TSNI"
          [ testProperty "non terminating" $ prop_TSNI nonTerminating
          ]
      ]

prop_TINI :: (forall c. While c => c) -> Property ()
prop_TINI p = do
  let secMap = Map.fromList [("l", Low), ("h", High)]
  (ctx, env1, env2) <- gen' genEnv

  case typeCheckIn ctx p of
    Left err -> testFailed $ showPretty err
    Right typedP -> do
      case checkSecurity secMap typedP of
        Left err -> do
          info $ showPretty err
          return ()
        Right () -> do
          let succeeded = tini BigStep.evalIn (fromRight undefined $ typeCheckIn ctx p) secMap env1 env2
          unless succeeded $ testFailed "TINI violation"

prop_TSNI :: (forall c. While c => c) -> Property ()
prop_TSNI p = do
  let secMap = Map.fromList [("l", Low), ("h", High)]
  (ctx, env1, env2) <- gen' genEnv

  case typeCheckIn ctx p of
    Left err -> testFailed $ showPretty err
    Right typedP -> do
      case checkSecurity secMap typedP of
        Left err -> do
          testFailed $ showPretty err
        -- return ()
        Right () -> do
          let succeeded = tsni BigStep.evalIn (fromRight undefined $ typeCheckIn ctx p) secMap env1 env2
          unless succeeded $ testFailed "TSNI violation"

gen' :: Pretty a => Gen a -> Property' e a
gen' = genWith (Just . showPretty)

genEnv :: Gen (Context, Env, Env)
genEnv = do
  t_l <- genType
  v_l <- genValue t_l
  t_h <- genType
  v_h1 <- genValue t_h
  v_h2 <- genValue t_h
  let lowEnv = Env $ Map.fromList [("l", v_l)]
  let highEnv1 = Env $ Map.fromList [("h", v_h1)]
  let highEnv2 = Env $ Map.fromList [("h", v_h2)]
  let ctx = Context $ Map.fromList [("l", t_l), ("h", t_h)]
  return (ctx, lowEnv <> highEnv1, lowEnv <> highEnv2)

genType :: Gen Type
genType = pure TBool -- Gen.choose (pure TBool) (pure TInt)

genValue :: Type -> Gen Value
genValue = \case
  TBool -> VBool <$> Gen.bool False
  TInt -> VInt <$> Gen.int (Range.between (-100, 100))
