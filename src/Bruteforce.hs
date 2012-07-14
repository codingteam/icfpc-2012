module Bruteforce (
  bruteforce, output
) where

import Control.Concurrent
import Control.Concurrent.STM
import Lifter
import Emulator
import Handler

actionToChar :: Action -> Char
actionToChar action =
  case action of
    ALeft  -> 'L'
    ARight -> 'R'
    AUp    -> 'U'
    ADown  -> 'D'
    AWait  -> 'W'
    AAbort -> 'A'

output :: (Int, [Actions]) -> IO ()
output (_, actions) =
  putStrLn $ map actionToChar actions

bruteforce :: Int -> GameState -> TVar (Int, [Actions]) -> IO ()
bruteforce  = undefined