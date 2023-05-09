{-# LANGUAGE OverloadedStrings #-}

module Main where

import System.IO qualified as System

import Data.Text qualified as T
import Data.Text.IO qualified as TIO

import Prettyprinter
import Prettyprinter.Render.Text (renderIO)

import Language.While.Eval.BigStep (runEval)
import Language.While.Render (renderStm)

import Examples

main :: IO ()
main = do
  let prog = implicitFlow

  labeled "Program" $ do
    putDocLn $ renderStm prog

  labeled "Evaluation" $ do
    putDocLn $ case runEval prog of
      Left err -> "ERROR:" <+> pretty err
      Right env -> pretty env

putDocLn :: Doc ann -> IO ()
putDocLn =
  renderIO System.stdout
    . layoutSmart defaultLayoutOptions
    . (<> line)

labeled :: T.Text -> IO () -> IO ()
labeled heading action = do
  let len = T.length heading
  TIO.putStrLn $ "\n# " <> heading <> " " <> T.replicate (80 - len) "#"
  action
