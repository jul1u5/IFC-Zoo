{-# LANGUAGE OverloadedStrings #-}

module Main where

import System.IO qualified as System

import Data.Bool (bool)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO

import Prettyprinter
import Prettyprinter.Render.Text (renderIO)

import Language.While.Eval.BigStep qualified as BigStep
import Language.While.Eval.SmallStep qualified as SmallStep
import Language.While.Render (renderStm)

import Examples

main :: IO ()
main = do
  let prog = implicitFlow

  heading "Program" $ do
    putDocLn $ renderStm prog

  let bigStepEnv = BigStep.runEval prog
      smallStepEnv = SmallStep.runEval prog

  heading "Evaluation" $ do
    subheading "Big-step" $ do
      putDocLn $ case bigStepEnv of
        Left err -> "ERROR:" <+> pretty err
        Right env -> pretty env

    subheading "Small-step" $ do
      putDocLn $ case smallStepEnv of
        Left err -> "ERROR:" <+> pretty err
        Right env -> pretty env

    subheading "The same?" $ do
      putDocLn $ bool "No 🤨" "Yes 😇" $ bigStepEnv == smallStepEnv

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
