{-# LANGUAGE OverloadedStrings #-}

module Main where

import System.IO qualified as System

import Data.Bool (bool)
import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import Data.Text.IO qualified as TIO

import Prettyprinter
import Prettyprinter.Render.Text (renderIO)

import Language.While.Eval.BigStep qualified as BigStep
import Language.While.Eval.Env
import Language.While.Eval.SmallStep qualified as SmallStep
import Language.While.Eval.Value
import Language.While.Examples
import Language.While.Render (renderCmd)

main :: IO ()
main = do
  let prog = implicitFlow

  heading "Program" $ do
    putDocLn $ renderCmd prog

  let initalEnv = Env $ Map.fromList [("l", VBool False), ("h", VBool True)]
      bigStepEnv = BigStep.evalIn initalEnv prog
  -- smallStepEnv = SmallStep.evalIn initalEnv prog

  heading "Evaluation" $ do
    subheading "Big-step" $ do
      putDocLn $ case bigStepEnv of
        Left err -> "ERROR:" <+> pretty err
        Right env -> pretty env

-- subheading "Small-step" $ do
--   putDocLn $ case smallStepEnv of
--     Left err -> "ERROR:" <+> pretty err
--     Right env -> pretty env

-- subheading "The same?" $ do
--   putDocLn $ bool "No 🤨" "Yes 😇" $ bigStepEnv == smallStepEnvnv

putDocLn :: Doc ann -> IO ()
putDocLn =
  renderIO System.stdout
    . layoutSmart defaultLayoutOptions
    . (<> line)

heading :: T.Text -> IO () -> IO ()
heading title action = do
  let len = T.length title
  TIO.putStrLn $ "\n# " <> title <> " " <> T.replicate (80 - len) "#"
  action

subheading :: T.Text -> IO () -> IO ()
subheading title action = do
  TIO.putStrLn $ "\n## " <> title
  action
