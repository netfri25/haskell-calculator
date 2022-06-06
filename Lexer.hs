module Lexer where

import Token
import Parser

import Data.Char (isSpace)
import Data.Kind (Type)
import Control.Monad (join)
import Control.Applicative (Alternative(..))

type Lexer :: Type -> Type
type Lexer = Parser Char

-- | whitespace lexer
ws :: Lexer String
ws = spanP isSpace

operators :: [(Char, Op)]
operators =
  [ ('+', Add)
  , ('-', Sub)
  , ('*', Mul)
  , ('/', Div)
  ]

-- | lexer for the operator token
tknOperatorL :: Lexer Token
tknOperatorL = unwrap $ (TknOperator <$>) . flip lookup operators <$> ifP (`elem` map fst operators)

-- | lexer for the number token
tknNumberL :: Lexer Token
tknNumberL = TknNumber . read <$> nonEmpty (spanP (`elem` ['0'..'9']))

tknBracketL :: Lexer Token
tknBracketL = lbracket <|> rbracket
  where
    lbracket, rbracket :: Lexer Token
    lbracket = LBracket <$ ifP (=='(')
    rbracket = RBracket <$ ifP (==')')

-- | lexer for every possible token
tokenL :: Lexer Token
tokenL = foldl1 (<|>)
  [ tknOperatorL
  , tknNumberL
  , tknBracketL
  ]

-- | the actual lexer
lexer :: String -> Maybe [Token]
lexer "" = Just []
lexer inp =
  case runParser (ws *> tokenL) inp of
    Nothing -> Nothing
    Just (x, inp') -> (x:) <$> lexer inp'
