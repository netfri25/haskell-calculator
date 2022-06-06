module Main where

import Control.Concurrent (threadDelay, MVar, newEmptyMVar, putMVar, tryTakeMVar)

import Data.Maybe (isJust)

import System.IO (hSetBuffering, BufferMode(NoBuffering), stdout)
import System.Posix.Signals (installHandler, Handler(Catch), sigINT, sigTERM)


main :: IO ()
main = do
  stop <- newEmptyMVar
  hSetBuffering stdout NoBuffering
  _ <- installHandler sigINT (Catch $ onSigInt stop) Nothing
  _ <- installHandler sigTERM (Catch $ onSigTerm stop) Nothing
  loop stop


loop :: MVar () -> IO ()
loop stop = do
  stopRequested <- isJust <$> tryTakeMVar stop
  if stopRequested
    then putStrLn "bye!"
    else do
      threadDelay 10000
      putStr "."
      loop stop


onSigInt :: MVar () -> IO ()
onSigInt stop = do
  putStrLn "got sigINT"
  putMVar stop ()


onSigTerm :: MVar () -> IO ()
onSigTerm stop = do
  putStrLn "got sigTERM"
  putMVar stop ()
