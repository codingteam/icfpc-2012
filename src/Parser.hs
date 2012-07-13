
module Parser where

import System.IO

import Lifter

forEachLine :: (String -> IO a) -> IO [a]
forEachLine fn = go []
  where
    go acc = do
      eof <- hIsEOF stdin
      if eof
        then return acc
        else do
             line <- getLine
             if null line
               then return acc
               else do
                    res <- fn line
                    next <- go acc
                    return (res: next)

readInput :: IO MineState
readInput = forEachLine (return . map cell)
  where
    cell ch = case ch of ' ' -> Empty
                         '.' -> Earth
                         '#' -> Wall
                         '*' -> Rock
                         '\\' -> Lambda
                         'R' -> Robot
                         'L' -> ClosedLift
                         other -> error ("Unknown character: " ++ [other])
