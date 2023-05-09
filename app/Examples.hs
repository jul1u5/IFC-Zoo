{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}

module Examples where

import Language.While.Abstract

implicitFlow =
  do_
    [ "l" .= bool_ False
    , "h" .= bool_ True
    , if_
        (var_ "h")
        (Then $ "l" .= bool_ True)
        (Else skip_)
        -- , "l" .= bool_ False
    ]
