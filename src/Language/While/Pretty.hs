module Language.While.Pretty (showPretty, Pretty) where

import Prettyprinter

showPretty :: Pretty a => a -> String
showPretty = show . pretty
