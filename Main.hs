module Main where

import Lexer
import Ast
import Eval

import System.Exit (exitSuccess)
import Control.Monad (forever, when, (>=>))
import System.IO (stdout, stdin, BufferMode (..), hSetBuffering)

main :: IO ()
main = do
  hSetBuffering stdin LineBuffering
  hSetBuffering stdout NoBuffering
  forever $ do
    putStr "> "
    inp <- getLine
    when (inp == "exit" || inp == "quit") exitSuccess
    case evaluateLine inp of
      Left err -> putStrLn $ "[ERROR] " ++ err
      Right value -> print value

lexingError = "Unknown character found in input"
parsingError = "Syntax error"
evalError = "Unable to evaluate the expression"

evaluateLine :: String -> Either String Float
evaluateLine = err lexingError . lexer >=> err parsingError . ast >=> err evalError . eval

err :: e -> Maybe a -> Either e a
err err Nothing = Left err
err _  (Just x) = Right x
