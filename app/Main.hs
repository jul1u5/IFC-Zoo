{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import System.IO qualified as System

import Control.Monad ((>=>))
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Trans.Except

import Data.Bool (bool)
import Data.Either.Combinators (mapLeft)
import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import Data.Text.IO qualified as TIO

import Prettyprinter
import Prettyprinter.Render.Text (renderIO)

import Language.While.Eval
import Language.While.Eval.BigStep qualified as BigStep
import Language.While.Eval.SmallStep qualified as SmallStep
import Language.While.Examples
import Language.While.Render
import Language.While.Secure
import Language.While.Typed

data SomeInstance c where
  SomeInstance :: c a => a -> SomeInstance c

data SomeInput c a where
  SomeInput :: c b => (b -> a) -> SomeInput c a

main :: IO ()
main = runWithPrettyErrors $ do
  let prog = explicitFlow
  -- let prog = implicitFlow

  heading "Program" $ do
    putDocLn $ renderCmd prog

  let initalEnv = Env $ Map.fromList [("l", VBool False), ("h", VBool True)]
      secMap = Map.fromList [("l", Low), ("h", High)]
      ctx = Context $ Map.fromList [("l", TBool), ("h", TBool)]

  (prog', prog'') <- heading "Type-checking" $ do
    except . mapLeft SomeInstance $ typeCheckIn ctx prog

  () <- heading "Flow-checking" $ do
    except . mapLeft SomeInstance $ checkFlow secMap prog'

  heading "Evaluation" $ do
    let (prog1, prog2) = prog''
        bigStepEnv = BigStep.evalIn initalEnv prog1
        smallStepEnv = SmallStep.evalIn initalEnv prog2

    subheading "Big-step" $ do
      putDocLn $ pretty bigStepEnv

    subheading "Small-step" $ do
      putDocLn $ pretty smallStepEnv

    subheading "The same?" $ do
      putDocLn $ bool "No 🤨" "Yes 😇" $ bigStepEnv == smallStepEnv

putDocLn :: MonadIO m => Doc ann -> m ()
putDocLn =
  liftIO
    . renderIO System.stdout
    . layoutSmart defaultLayoutOptions
    . (<> line)

heading :: MonadIO m => T.Text -> m a -> m a
heading title action = do
  let len = T.length title
  liftIO $ TIO.putStrLn $ "\n# " <> title <> " " <> T.replicate (80 - len) "#"
  action

subheading :: MonadIO m => T.Text -> m a -> m a
subheading title action = do
  liftIO $ TIO.putStrLn $ "\n## " <> title
  action

runWithPrettyErrors :: MonadIO m => ExceptT (SomeInstance Pretty) m () -> m ()
runWithPrettyErrors =
  runExceptT
    >=> either (\(SomeInstance err) -> putDocLn . ("ERROR:" <+>) . pretty $ err) pure
