module Bruteforce (
  bruteforce, output
) where

import Control.Concurrent
import Control.Concurrent.STM
import Lifter
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
  do putStr $ map actionToChar $ gmActions gameState
     putStr " "
     putStrLn $ show $ gmScore gameState

bruteforce :: Int -> GameState -> TVar GameState -> IO ()
bruteforce steps gameState var
  | steps <= 0           = return ()
  | gmFinished gameState = return ()
  | otherwise            =
    let handleAction action =
          let nextGameState = emulate gameState action
          in do bestGameState <- atomically $ readTVar var
                if gmScore bestGameState < gmScore nextGameState
                  then do atomically $ writeTVar var nextGameState
                          bruteforce (steps - 1) nextGameState var
                  else bruteforce (steps - 1) nextGameState var
    in do sequence_ $ map handleAction [ALeft, ARight, AUp, ADown, AWait, AAbort]

