module Parser where

import Control.Applicative (Alternative(..))
import Control.Monad ((>=>))
import Data.Monoid (Any(..))
import Data.Char (isSpace)

newtype Parser i a = Parser
  { runParser :: [i] -> Maybe (a, [i])
  }

instance Functor (Parser i) where
  fmap f (Parser p) = Parser $ p >=> \(x, inp) -> return (f x, inp)

instance Applicative (Parser i) where
  pure x = Parser $ \inp -> return (x, inp)
  Parser pf <*> Parser px = Parser $ \inp -> do
    (f, inp1) <- pf inp
    (x, inp2) <- px inp1
    return (f x, inp2)

instance Alternative (Parser i) where
  empty = Parser $ const Nothing
  Parser pa <|> Parser pb = Parser $ \inp -> pa inp <|> pb inp

-- | asserts that the parser isnt empty
nonEmpty :: Parser i [a] -> Parser i [a]
nonEmpty (Parser p) = Parser $ \inp -> do
  (xs, inp') <- p inp
  if null xs
  then Nothing
  else return (xs, inp')

-- | parses a character if it matches the predicate
ifP :: forall i.
       (i -> Bool) -- predicate
    -> Parser i i
ifP predicate = Parser f
  where
    f :: [i] -> Maybe (i, [i])
    f (x:xs) | predicate x = return (x, xs)
    f _ = Nothing

-- | like ifP, but many characters instead of one
spanP :: (i -> Bool) -- predicate
      -> Parser i [i]
spanP = many . ifP


-- | unwraps a parser that containes Maybe
unwrap :: Parser i (Maybe a)
       -> Parser i a
unwrap (Parser p) = Parser $ \inp -> do
  (mx, inp') <- p inp
  case mx of
    Nothing -> Nothing
    Just x -> return (x, inp')
