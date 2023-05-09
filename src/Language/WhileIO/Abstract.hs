{-# LANGUAGE TypeFamilies #-}

module Language.WhileIO.Abstract (module Language.While.Abstract, WhileIO (..), Level (..)) where

import Language.While.Abstract

data Level = Low | High

class WhileIO s where
  in_ :: Level -> Name -> s
  out_ :: Level -> Name -> s -> s
