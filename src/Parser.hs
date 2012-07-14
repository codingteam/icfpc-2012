
module Parser where

import Data.Char (isSpace)
import Core

charToCell :: Char -> Cell
charToCell ch = case ch of ' ' -> Empty
                           '.' -> Earth
                           '#' -> Wall
                           '*' -> Rock
                           '\\' -> Lambda
                           'R' -> Robot
                           'L' -> ClosedLift
                           other -> error ("Unknown character: " ++ [other])

extendLine size line = line ++ replicate n ' '
  where n = max 0 (length line - size)

padLines lines = map (extendLine maxLength) lines
  where maxLength = maximum $ map length lines

parseParameter mineState parameter =
  case name of
    "Water"      -> mineState {msWater = read rest}
    "Flooding"   -> mineState {msFlooding = read rest}
    "Waterproof" -> mineState {msWaterproof = read rest}
    _            -> mineState
  where (name, rest) = break isSpace parameter

readInput :: IO MineState
readInput = do (description, metadata) <- fmap (break null . lines) getContents
               let mineState = MineState {msField = map (map charToCell) $ padLines description,
                                          msWater = defaultWater,
                                          msFlooding = defaultFlooding,
                                          msWaterproof = defaultWaterproof}
               return (foldl parseParameter mineState metadata)
               