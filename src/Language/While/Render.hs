{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

module Language.While.Render where

import Data.Bool (bool)

import Prettyprinter (Doc, (<+>))
import Prettyprinter qualified as P

import Language.While.Abstract

data RenderStm = RenderStm {renderStm :: Doc (), isSkip :: Bool}

mkRender :: Doc () -> RenderStm
mkRender renderStm = RenderStm{renderStm, isSkip = False}

newtype RenderExpr = RenderExpr {renderExpr :: Doc ()}

type instance WhileExpr RenderStm = RenderExpr

instance While RenderStm where
  skip_ = RenderStm{renderStm = "skip" <> ";", isSkip = True}

  semicolon s1 s2 = mkRender $ renderStm s1 <> P.line <> renderStm s2

  if_ e (Then s1) (Else s2) =
    mkRender $
      P.sep $
        [ "if" <+> renderExpr e <+> "then"
        , indent <> renderStm s1
        ]
          <> ( if isSkip s2
                then []
                else
                  [ "else"
                  , indent <> renderStm s2
                  ]
             )

  while_ e s =
    mkRender $
      P.sep
        [ "while" <+> renderExpr e <+> "do"
        , indent <> renderStm s
        , "done"
        ]

  n .= e = mkRender $ P.pretty n <+> ":=" <+> renderExpr e <> ";"

instance Expr RenderExpr where
  bool_ = RenderExpr . bool "false" "true"

  int_ = RenderExpr . P.pretty

  var_ = RenderExpr . P.pretty . getName

  not_ e = RenderExpr $ "!" <> renderExpr e

  (+.) = binOp "+"
  (-.) = binOp "-"
  (*.) = binOp "*"

  (==.) = binOp "=="
  (<.) = binOp "<"
  (>.) = binOp ">"

indent :: Doc ann
indent = P.flatAlt "  " ""

binOp :: Doc () -> RenderExpr -> RenderExpr -> RenderExpr
binOp op e1 e2 = RenderExpr $ renderExpr e1 <+> op <+> renderExpr e2