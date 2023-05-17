module Language.While.Property.Divergence where

import System.IO.Unsafe (unsafePerformIO)
import System.Timeout (timeout)

import Language.While.Eval.Env

runOrDiverge :: (Env -> c -> Env) -> Env -> c -> Maybe Env
runOrDiverge evalIn env c = do
  unsafePerformIO $ timeout microseconds $ pure $! evalIn env c
 where
  microseconds = 1_000
