module Generator where

import Parser
import Token
import Eval

import System.IO.Unsafe (unsafePerformIO)
import System.Exit (exitFailure)
import Control.Monad (join)
import Data.Kind (Type)

type Gen :: Type -> Type
type Gen = Parser Token

-- | concatenates two nodes by their precendece, where the top
-- | is the node with the lowest precedence
concatNode :: Node -> Node -> Node
concatNode (Operator op l Nothing) (Number n) = Operator op l (Just $ Number n)
concatNode (Number n) (Operator op Nothing r) = Operator op (Just $ Number n) r
concatNode (Number n) (Operator op (Just l) r) = Operator op (Just $ concatNode (Number n) l) r
concatNode (Operator Sub Nothing r) b = concatNode (Operator Sub (Just $ Number 0) r) b
concatNode (Operator aop al Nothing) (Operator bop bl br) =
  case compare (prec aop) (prec bop) of
    LT -> Operator aop al (Just $ Operator bop bl br)
    _  -> Operator bop (Just $ Operator aop al bl) br
concatNode a b = error ("tried to concat unconcatable nodes:\n"
                         ++ unlines (map ('\t':) [show a, show b]))

inBrackets :: [Token] -> Maybe ([Token], [Token])
inBrackets tkns =
  let count = inBracks 1 (tail tkns) 0
  in case count of
    Nothing -> Nothing
    Just count -> Just (tail $ take count tkns, drop (count+1) tkns)
  where
    inBracks :: Int -> [Token] -> Int -> Maybe Int
    inBracks 0 _ counter = Just counter
    inBracks _ [] _ = Nothing
    inBracks depth (LBracket:xs) counter = inBracks (depth+1) xs (counter+1)
    inBracks depth (RBracket:xs) counter = inBracks (depth-1) xs (counter+1)
    inBracks depth (_:xs) counter = inBracks depth xs (counter+1)

generator :: [Token] -> Maybe Node
generator tkns =
  let nodes = sequence $ init $ asNode tkns -- NOTE: using init because the last one will always be a Nothing
  in case nodes of
    Nothing -> Nothing
    Just nodes -> Just $ foldr1 concatNode nodes
  where
    asNode :: [Token] -> [Maybe Node]
    asNode [] = Nothing : []
    asNode (TknOperator op : TknOperator Sub : TknNumber n:xs) = Just (Operator op Nothing Nothing) : Just (Number (0-n)) : asNode xs
    asNode (TknNumber n:xs) = Just (Number n) : asNode xs
    asNode (TknOperator op:xs) = Just (Operator op Nothing Nothing) : asNode xs
    asNode xs@(LBracket:_) =
      let res = inBrackets xs
      in case res of
        Nothing -> Nothing : []
        Just (inbrack, outbrack) -> (join ((Number <$>) . eval <$> generator inbrack)) : asNode outbrack
