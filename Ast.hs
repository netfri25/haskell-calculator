module Ast (ast) where

import Parser
import Token
import Expr

import Control.Monad (mfilter)
import Control.Applicative (Alternative(..))
import Debug.Trace (traceM)

type Gen = Parser [Token] Expr

number :: Gen
number = create <$> ifP isNumber
  where create (TknNumber n) = Number n

unary :: Gen
unary = create <$> ifP (isOperator 0) <*> primaryExpr
  where create (TknOperator op) = Unary op

paren :: Gen
paren = ifP (==TknLBracket) *> expr 0 <* ifP (==TknRBracket)

primaryExpr :: Gen
primaryExpr = number <|> unary <|> paren

expr :: Int -> Gen
expr prec = (primaryExpr >>= flip expr' prec) <|> primaryExpr
  where
    expr' :: Expr -> Int -> Gen
    expr' lhs prec = do
      TknOperator op <- ifP (isOperator prec)
      let prec' = opPrec op
      rhs <- expr prec'
      let res = Binary op lhs rhs
      expr' res prec <|> return res

ast :: [Token] -> Maybe Expr
ast = fmap fst . mfilter (null . snd) . runParser (expr 0)
