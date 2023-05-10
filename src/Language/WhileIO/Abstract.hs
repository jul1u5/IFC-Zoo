{-# LANGUAGE TypeFamilies #-}

module Language.WhileIO.Abstract (module Language.While.Abstract, WhileIO (..), Level (..)) where

import Language.While.Abstract

data Level = Low | High

class WhileIO c where
  in_ :: Level -> Name -> c
  out_ :: Level -> Name -> c -> c
