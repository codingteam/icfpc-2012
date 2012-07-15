module Bruteforce (
  bruteforce, output
) where

import Control.Monad
import Control.Concurrent
import Control.Concurrent.STM

import System.Random
import Data.Array.IO

import Core
import Emulator

actionToChar :: Action -> Char
actionToChar action =
  case action of
    ALeft  -> 'L'
    ARight -> 'R'
    AUp    -> 'U'
    ADown  -> 'D'
    AWait  -> 'W'
    AAbort -> 'A'
    ARazor -> 'S'

output :: GameState -> IO ()
output gameState =
  do putStr $ map actionToChar $ gsActions gameState

abortGame :: GameState -> GameState
abortGame state =
    if gsFinished state
    then state
    else emulate state AAbort

bruteforce :: Int -> GameState -> TVar GameState -> IO ()
bruteforce steps gameState var
  | steps <= 0           = return ()
  | gsFinished gameState = return ()
  | otherwise            =
    let handleAction action =
          let nextGameState = emulate gameState action
              abortedNextGameState = abortGame nextGameState
          in do bestGameState <- atomically $ readTVar var
                if gsScore bestGameState < gsScore abortedNextGameState
                  then do atomically $ writeTVar var $ abortedNextGameState
                          bruteforce (steps - 1) nextGameState var
                  else bruteforce (steps - 1) nextGameState var
        availableActions = [ALeft, ARight, AUp, ADown, AWait, AAbort, ARazor]
    in do validActions <- shuffle $ filter (validate gameState) availableActions
          sequence_ $ map handleAction validActions

shuffle :: [a] -> IO [a]
shuffle xs =
  do ar <- newArray n xs
     forM [1 .. n] $ \i ->
       do j <- randomRIO (i, n)
          vi <- readArray ar i
          vj <- readArray ar j
          writeArray ar j vi
          return vj
  where n = length xs
        newArray :: Int -> [a] -> IO (IOArray Int a)
        newArray n xs = newListArray (1, n) xs