{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}

module Demo where

import Data.Map.Strict qualified as Map

import Language.While.Abstract
import Language.While.Eval
import Language.While.Secure

secMap :: SecurityMap
secMap =
  SecMap $
    Map.fromList
      [ ("l", Low)
      , ("h", High)
      ]

initialEnv :: Env
initialEnv =
  Env $
    Map.fromList
      [ ("l", VBool False)
      , ("h", VBool True)
      ]

{-
  skip;
-}
demo = skip_

{-
  l := h; // Explicit flow
-}
-- demo = "l" .= var_ "h"

{-
  if h then // Implicit flow
    l := true;
  else
    skip;
-}
-- demo =
--   if_
--     (var_ "h")
--     (Then $ "l" .= bool_ True)
--     (Else skip_)
