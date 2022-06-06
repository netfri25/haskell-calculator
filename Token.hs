{-# LANGUAGE TypeApplications #-}
module Token where

-- | used for operator precedence
class Prec a where
  prec :: a -> Int

data Op
  = Add
  | Sub
  | Mul
  | Div
  deriving (Show, Eq)

instance Prec Op where
  prec Add = 1
  prec Sub = 1
  prec Mul = 2
  prec Div = 2

data Token
  = TknOperator Op
  | TknNumber Float
  | LBracket
  | RBracket
  deriving (Show)

instance Eq Token where
  TknNumber _ == TknNumber _ = True
  TknOperator op1 == TknOperator op2 = op1 == op2
  _ == _ = False

-- | Node of a binary tree
-- | the operator woth the lowest precedence should be at the top
data Node
  = Operator Op (Maybe Node) (Maybe Node)
  | Number Float
  deriving (Show)

instance Prec Node where
  prec (Operator op _ _) = prec op
  prec (Number _) = 9

