{-# LANGUAGE TypeFamilies #-}

module Language.WhileIO.Abstract (module Language.While.Abstract, WhileIO (..), Level (..)) where

import Language.While.Abstract
import Language.While.Secure

class WhileIO c where
  in_ :: Level -> Name -> c
  out_ :: Level -> Name -> c -> c
