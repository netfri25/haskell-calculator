module Eval where

import Token
import Expr

eval :: Expr -> Maybe Float
eval (Number n) = Just n
eval (Binary op lhs rhs) = do
  func <- lookup op binaryOpTable
  l <- eval lhs
  r <- eval rhs
  return $ func l r
eval (Unary op rhs) = do
  func <- lookup op unaryOpTable
  r <- eval rhs
  return $ func r

binaryOpTable :: [(Op, Float -> Float -> Float)]
binaryOpTable =
  [ (Add, (+))
  , (Sub, (-))
  , (Mul, (*))
  , (Div, (/))
  ]

unaryOpTable :: [(Op,Float -> Float)]
unaryOpTable =
  [ (Add, id)
  , (Sub, negate)
  ]
