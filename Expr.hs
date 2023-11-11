module Expr
  ( Expr(..)
  , exprPrec
  ) where

import Token (Op, opPrec)

data Expr
  = Number Float
  | Unary Op Expr
  | Binary Op Expr Expr
  deriving Show

exprPrec :: Expr -> Int
exprPrec (Number _) = 9
exprPrec (Unary op _) = opPrec op
exprPrec (Binary op _ _) = opPrec op
