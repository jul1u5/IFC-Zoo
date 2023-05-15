module Language.While.Property.Divergence where

import System.IO.Unsafe (unsafePerformIO)
import System.Timeout (timeout)

import Language.While.Eval.Env

runOrDiverge :: (Env -> c -> Env) -> Env -> c -> Maybe Env
runOrDiverge evalIn env c = do
  checkDivergence $ evalIn env c
 where
  checkDivergence x = unsafePerformIO $ timeout microseconds $ pure $! x
  microseconds = 1_000
