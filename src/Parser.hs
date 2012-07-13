
module Parser where

import System.IO

import Lifter

forEachLine :: (Int -> String -> IO a) -> IO [a]
forEachLine fn = go [] 0
  where
    go acc lineLength = do
      eof <- hIsEOF stdin
      if eof
        then return acc
        else do
             line <- getLine
             let n = if lineLength == 0
                       then length line
                       else lineLength
             if null line
               then return acc
               else do
                    res <- fn n line
                    next <- go acc n
                    return (res: next)

readInput :: IO MineState
readInput = forEachLine $ \n line -> return (map cell $ pad n line)
  where
    pad n str
        | length str >= n = str
        | otherwise       = str ++ replicate (n - length str) ' '

    cell ch = case ch of ' ' -> Empty
                         '.' -> Earth
                         '#' -> Wall
                         '*' -> Rock
                         '\\' -> Lambda
                         'R' -> Robot
                         'L' -> ClosedLift
                         other -> error ("Unknown character: " ++ [other])
