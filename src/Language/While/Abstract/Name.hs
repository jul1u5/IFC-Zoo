module Language.While.Abstract.Name where

import Data.String (IsString)
import Data.Text (Text)

import Prettyprinter (Pretty)

newtype Name = Name {getName :: Text}
  deriving (Eq, Ord, Show, IsString, Pretty)
