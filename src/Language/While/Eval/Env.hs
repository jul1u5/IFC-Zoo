{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}

module Language.While.Eval.Env where

import Data.Map qualified as Map
import Data.Map.Strict (Map)

import Prettyprinter (Pretty, pretty, (<+>))
import Prettyprinter qualified as P

import Language.While.Abstract qualified as A
import Language.While.Eval.Type
import Language.While.Eval.Value

import Language.While.Typed.Expr
import Unsafe.Coerce (unsafeCoerce)

newtype Env = Env {vars :: Map A.Name Value}
  deriving (Eq, Show, Semigroup, Monoid)

instance Pretty Env where
  pretty Env{vars} =
    P.encloseSep "{" "}" (";" <> P.line)
      . map (\(key, value) -> pretty key <+> "|->" <+> pretty value)
      . Map.toList
      $ vars

data SValue (t :: Type) where
  SVInt :: Int -> SValue 'TInt
  SVBool :: Bool -> SValue 'TBool

data SomeSValue where
  SomeSValue :: SValue t -> SomeSValue

toSValue :: Value -> SomeSValue
toSValue (VInt i) = SomeSValue (SVInt i)
toSValue (VBool b) = SomeSValue (SVBool b)

fromSValue :: SValue t -> Value
fromSValue = \case
  SVBool b -> VBool b
  SVInt i -> VInt i

empty :: Env
empty = mempty

lookup :: forall t. Name t -> Env -> SValue t
lookup (Name x) env = case toSValue $ lookupUnsafe x env of
  SomeSValue @t' v -> unsafeCoerce @(SValue t') @(SValue t) v

lookupUnsafe :: A.Name -> Env -> Value
lookupUnsafe x env = case Map.lookup x $ vars env of
  Nothing -> error $ "undefined variable: " <> show x
  Just v -> v

update :: Name t -> SValue t -> Env -> Env
update (Name x) v = Env . Map.insert x (fromSValue v) . vars
