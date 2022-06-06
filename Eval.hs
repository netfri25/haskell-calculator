module Eval where

import Token
import Data.Maybe (fromJust)

type OpFunc = Float -> Float -> Float

operators :: [(Op,OpFunc)]
operators =
  [ (Add, (+))
  , (Sub, (-))
  , (Mul, (*))
  , (Div, (/))
  ]

eval :: Node -> Maybe Float
eval (Number n) = Just n
eval (Operator Sub Nothing (Just r)) = eval r >>= return . (0-)
eval (Operator op (Just l) (Just r)) = do
  l' <- eval l
  r' <- eval r
  return $ (fromJust $ lookup op operators) l' r'

eval e = error $ "unable to evaluate:\n" ++ show e
