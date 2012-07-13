
module Parser where

import Lifter

charToCell :: Char -> Cell
charToCell ch = case ch of ' ' -> Empty
                           '.' -> Earth
                           '#' -> Wall
                           '*' -> Rock
                           '\\' -> Lambda
                           'R' -> Robot
                           'L' -> ClosedLift
                           other -> error ("Unknown character: " ++ [other])

padLine size line = line ++ replicate n ' '
  where n = max 0 (length line - size)

readInput :: IO MineState
readInput = do inputLines <- fmap lines getContents
               let maxLength = maximum $ map length inputLines
               let paddedLines = map (padLine maxLength) inputLines
               return $ map (map charToCell) paddedLines