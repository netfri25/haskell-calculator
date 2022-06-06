module Main where

import Generator
import Lexer
import Eval

import System.Exit (exitFailure, exitSuccess)
import Control.Monad (when, forever)
import Data.Maybe (isNothing, fromJust)
import GHC.IO.Handle
import GHC.IO.StdHandles

main :: IO ()
main = forever $ do
  hSetBuffering stdin LineBuffering
  inp <- getLine
  when (inp == "exit" || inp == "quit") exitSuccess
  case lexer inp of
    Nothing -> putStrLn "[LEXING ERROR] unknown character found in input"
    Just tkns ->
      case generator tkns of
        Nothing -> putStrLn "[PARSING ERROR] possibly unmatced brackets or syntax error"
        Just node ->
          case eval node of
            Nothing -> putStrLn "[EVAL ERROR] unable to evaluate the expression"
            Just val -> print val
