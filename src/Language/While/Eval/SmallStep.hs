{-# LANGUAGE TypeFamilies #-}

module Language.While.Eval.SmallStep where

import Control.Monad.Except
import Control.Monad.State.Strict
import Control.Monad.Trans.Except (except)

import Data.Functor ((<&>))

import Language.While.Abstract.Command
import Language.While.Eval.Env (Env)
import Language.While.Eval.Env qualified as Env
import Language.While.Eval.Expr qualified as Expr
import Language.While.Eval.Type
import Language.While.Eval.Value

-- data EvalTransition
--   = Term
--   | NonTerm Eval

-- type M = StateT Env (Except EvalError)

-- newtype Eval = Eval {evalCmd :: M EvalTransition}

-- eval :: Eval -> Either EvalError Env
-- eval = evalIn Env.empty

-- evalIn :: Env -> Eval -> Either EvalError Env
-- evalIn env =
--   runExcept
--     . flip runStateT env
--     . evalCmd
--     >=> \case
--       (Term, env') -> return env'
--       (NonTerm c, env') -> evalIn env' c

-- evalExpr :: Expr.Eval -> M Value
-- evalExpr e = do
--   env <- get
--   lift $ except $ Expr.eval env e

-- type instance WhileExpr Eval = Expr.Eval

-- instance While Eval where
--   skip_ = Eval $ return Term

--   semicolon c1 c2 =
--     Eval $
--       evalCmd c1 <&> \case
--         NonTerm c1' -> NonTerm $ c1' `semicolon` c2
--         Term -> NonTerm c2

--   if_ e (Then c1) (Else c2) =
--     Eval $
--       evalExpr e >>= expectBool <&> \case
--         True -> NonTerm c1
--         False -> NonTerm c2

--   while_ e c =
--     Eval $
--       evalExpr e >>= expectBool <&> \case
--         True -> NonTerm $ c `semicolon` while_ e c
--         False -> Term

--   x .= e = Eval $ do
--     v <- evalExpr e
--     modify' $ Env.update x v
--     return Term
