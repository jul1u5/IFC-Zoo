{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

module Language.While.Property.TINI where

import Data.Functor ((<&>))
import Language.While.Eval.Env
import Language.While.Property.Divergence
import Language.While.Property.LowEq
import Language.While.Secure.Level
import Language.While.Typed
import Prettyprinter (Doc, pretty, (<+>))
import Prettyprinter qualified as P

tini ::
  While c =>
  (Env -> c -> Env) ->
  c ->
  SecurityMap ->
  Env ->
  Env ->
  Maybe (Doc ())
tini evalIn c secMap env1 env2 = do
  env1' <- runOrDiverge evalIn env1 c
  env2' <- runOrDiverge evalIn env2 c
  checkLowEqEnv secMap env1' env2' <&> \err ->
    P.vsep
      [ "Execution in initially low-equivalent environments:"
      , ""
      , P.indent 2 $
          "env11"
            <+> "="
            <+> pretty env1
              <> P.line
              <> "env12"
            <+> "="
            <+> pretty env2
      , ""
      , "resulted in low-inequivalent environments:"
      , ""
      , P.indent 2 $ "env21" <+> "=" <+> err <+> "=" <+> "env22"
      , ""
      , "Evaluation:"
      , ""
      , P.indent 2 $
          "<" <> "c" <> ","
            <+> "env11"
            <+> "="
            <+> pretty env1 <> ">"
            <+> "=>"
              <> P.line
              <> P.indent 4 ("env21" <+> "=" <+> P.pretty env1')
      , ""
      , P.indent 2 $
          "<" <> "c" <> ","
            <+> "env12"
            <+> "="
            <+> pretty env2 <> ">"
            <+> "=>"
              <> P.line
              <> P.indent 4 ("env22" <+> "=" <+> P.pretty env2')
      , ""
      ]
