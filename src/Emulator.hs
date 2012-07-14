module Emulator (
    emulate
) where

import Data.List (findIndex)
import Data.Maybe (fromJust)
import Lifter

type Point = (Int, Int)

lambdaScore = 25
lambdaAbortScore = 25

emulate :: GameState -> Action -> GameState
emulate state action =
    let state'  = processAction state action
        state'' = processEnvironment state'
    in  processFinishConditions state''

processAction :: GameState -> Action -> GameState
processAction state action =
    let field = gmMineState state
        lambdas = gmLambdas state
        score = gmScore state

        (field', robotPos') = moveRobot field action

        lambdaDelta = if hasObject field robotPos' Lambda then 1 else 0
        lambdas' = lambdas + lambdaDelta

        scoreDelta  = lambdaDelta * lambdaScore + if action == AAbort
                                                  then lambdas' * lambdaAbortScore
                                                  else -1
        score' = score + scoreDelta

        finished = action == AAbort
    in  GameState { gmMineState = field',
                    gmLambdas   = lambdas',
                    gmScore     = score',
                    gmFinished  = finished }

processEnvironment :: GameState -> GameState
processEnvironment state =
    let finished = gmFinished state
    in  if finished
        then state
        else state -- TODO: Falling rocks, opening lifts.

processFinishConditions :: GameState -> GameState
processFinishConditions state = state -- TODO: Check win and lose conditions.

moveRobot :: MineState -> Action -> (MineState, Point)
moveRobot field action =
    let position  = findRobot field
        position' = getRobotPosition field action position
        field'    = replaceCell field position Empty
        field''   = replaceCell field' position' Robot
    in  (field'', position')

hasObject :: MineState -> Point -> Cell -> Bool
hasObject field (x, y) object = field !! y !! x == object

findRobot :: MineState -> Point
findRobot field =
    let rowIndex = fromJust $ findIndex hasRobot field
        row = (field !! rowIndex)
        cellIndex = findIndex (\c -> c == Robot) row
    in  (rowIndex, fromJust $ cellIndex)
    where hasRobot :: [Cell] -> Bool
          hasRobot cells = any (\c -> c == Robot) cells

getRobotPosition :: MineState -> Action -> Point -> Point
getRobotPosition field action (x, y) =
    case action of
        AWait  -> (x, y)
        AAbort -> (x, y)
        ALeft  -> (x - 1, y) -- TODO: Check left side.
        ARight -> (x + 1, y) -- TODO: Check right side.

replaceCell :: MineState -> (Int, Int) -> Cell -> MineState
replaceCell field (x, y) cell =
    mapi (\row y' -> mapi (\cell' x' ->
            if (x, y) == (x', y') then cell else cell') row) field
    where mapi :: (a -> Int -> a) -> [a] -> [a]
          mapi fn xs = zipWith fn xs [0..]
