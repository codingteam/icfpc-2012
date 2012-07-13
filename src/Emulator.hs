module Emulator (
    emulate
) where

import Data.List (findIndex)
import Data.Maybe (fromJust)
import Lifter

type Point = (Int, Int)

lambdaScore = 25

emulate :: GameState -> Action -> GameState
emulate state action =
    let state'  = processAction state action
        state'' = processEnvironment state'
    in  processFinishConditions state''

processAction :: GameState -> Action -> GameState
processAction state action =
    let field = gmMineState state

        (field', robotPos') = moveRobot field action
        lambdaDelta = getLambdas field robotPos'
        scoreDelta  = lambdaDelta * lambdaScore - 1

        lambdas = gmLambdas state + lambdaDelta
        score = gmScore state + scoreDelta

        finished = action == AAbort
    in  GameState { gmMineState = field',
                    gmLambdas   = lambdas,
                    gmScore     = score,
                    gmFinished  = finished }

processEnvironment :: GameState -> GameState
processEnvironment state = state -- TODO: Falling rocks, opening lifts.

processFinishConditions :: GameState -> GameState
processFinishConditions state = state -- TODO: Check win and lose conditions.

moveRobot :: MineState -> Action -> (MineState, Point)
moveRobot field action =
    let position  = findRobot field
        position' = getRobotPosition field action position
        field'    = replaceCell field position Empty
        field''   = replaceCell field' position' Robot
    in  (field'', position')

getLambdas :: MineState -> Point -> Int
getLambdas field (x, y) = if field !! y !! x == Lambda then 1 else 0

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
        ALeft  -> (x - 1, y) -- TODO: Check left side
        ARight -> (x + 1, y)

replaceCell :: MineState -> (Int, Int) -> Cell -> MineState
replaceCell field (x, y) cell =
    mapi (\row y' -> mapi (\cell' x' ->
            if (x, y) == (x', y') then cell else cell') row) field
    where mapi :: (a -> Int -> a) -> [a] -> [a]
          mapi fn xs = zipWith fn xs [0..]
