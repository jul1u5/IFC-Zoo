{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

module Language.While.Render where

import Data.Bool (bool)

import Prettyprinter (Doc, (<+>))
import Prettyprinter qualified as P

import Language.While.Abstract

data RenderCmd = RenderCmd {renderCmd :: Doc (), isSkip :: Bool}

mkRender :: Doc () -> RenderCmd
mkRender renderCmd = RenderCmd{renderCmd, isSkip = False}

newtype RenderExpr = RenderExpr {renderExpr :: Doc ()}

type instance WhileExpr RenderCmd = RenderExpr

instance While RenderCmd where
  skip_ = RenderCmd{renderCmd = "skip" <> ";", isSkip = True}

  semicolon c1 c2 = mkRender $ renderCmd c1 <> P.line <> renderCmd c2

  if_ e (Then c1) (Else c2) =
    mkRender $
      P.sep $
        [ "if" <+> renderExpr e <+> "then"
        , indent <> renderCmd c1
        ]
          <> ( if isSkip c2
                then []
                else
                  [ "else"
                  , indent <> renderCmd c2
                  ]
             )

  while_ e c =
    mkRender $
      P.sep
        [ "while" <+> renderExpr e <+> "do"
        , indent <> renderCmd c
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
