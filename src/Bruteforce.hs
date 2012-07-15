module Bruteforce (
  bruteforce, output
) where

import Control.Concurrent
import Control.Concurrent.STM
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

output :: GameState -> IO ()
output gameState =
  do putStr $ map actionToChar $ gsActions gameState
     putStr " "
     putStrLn $ show $ gsScore gameState

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
          in do bestGameState <- atomically $ readTVar var
                if gsScore bestGameState < gsScore nextGameState
                  then do atomically $ writeTVar var $ abortGame nextGameState
                          bruteforce (steps - 1) nextGameState var
                  else bruteforce (steps - 1) nextGameState var
    in sequence_ $ map handleAction $ filter (validate gameState) [ALeft, ARight, AUp, ADown, AWait, AAbort]
