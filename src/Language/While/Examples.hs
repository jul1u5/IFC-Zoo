{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}

module Language.While.Examples where

import Language.While.Abstract

explicitFlow = "l" .= var_ "h"

implicitFlow =
  do_
    [ if_
        (var_ "h")
        (Then $ "l" .= bool_ True)
        (Else skip_)
    ]

nonTerminating =
  if_
    (var_ "l" ==. var_ "h")
    (Then $ while_ (bool_ True) skip_)
    (Else skip_)
