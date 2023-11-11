module Lexer where

import Token
import Parser

import Data.Char (isSpace, isDigit)
import Data.Kind (Type)
import Control.Monad (join, mfilter)
import Control.Applicative (asum, Alternative(..))
import Text.Read (readMaybe)

type Lexer :: Type -> Type
type Lexer = Parser [Char]

ws :: Lexer String
ws = spanP isSpace

operators :: [(Char, Op)]
operators =
  [ ('+', Add)
  , ('-', Sub)
  , ('*', Mul)
  , ('/', Div)
  ]

tknOperatorL :: Lexer Token
tknOperatorL = TknOperator <$> (nextP >>= lift . flip lookup operators)

-- TODO: add sign
tknNumberL :: Lexer Token
tknNumberL = TknNumber <$> (mfilter (not . null) (spanP valid) >>= lift . readMaybe)
  where valid c = isDigit c || elem c ['.', 'e', 'E']

tknBracketL :: Lexer Token
tknBracketL = lbracket <|> rbracket
  where
    lbracket = TknLBracket <$ ifP (=='(')
    rbracket = TknRBracket <$ ifP (==')')

tokenL :: Lexer Token
tokenL = asum [tknOperatorL, tknNumberL, tknBracketL]

lexer :: String -> Maybe [Token]
lexer = fmap fst . mfilter (null . snd) . runParser (some $ ws *> tokenL <* ws)
