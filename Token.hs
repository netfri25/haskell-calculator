module Token
  ( Op(..)
  , Token(..)
  , isOperator
  , isNumber
  , opPrec
  ) where

data Op = Add | Sub | Mul | Div
  deriving (Show, Eq)

opPrec :: Op -> Int
opPrec Add = 1
opPrec Sub = 1
opPrec Mul = 2
opPrec Div = 2

data Token
  = TknOperator Op
  | TknNumber Float
  | TknLBracket
  | TknRBracket
  deriving (Show, Eq)

isOperator :: Int -> Token -> Bool
isOperator prec (TknOperator op) = opPrec op >= prec
isOperator _ _ = False

isNumber :: Token -> Bool
isNumber (TknNumber _) = True
isNumber _ = False
