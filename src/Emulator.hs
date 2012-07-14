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
        else let field = gmMineState state
                 lambdas = gmLambdas state
                 score = gmScore state

                 field' = updateField field
             in  GameState { gmMineState = field',
                             gmLambdas   = lambdas,
                             gmScore     = score,
                             gmFinished  = finished }

processFinishConditions :: GameState -> GameState
processFinishConditions state = state -- TODO: Check win and lose conditions.

moveRobot :: MineState -> Action -> (MineState, Point)
moveRobot field action =
    let position  = findRobot field
        position' = getRobotPosition field action position
        field'    = replaceCell field position Empty
        field''   = replaceCell field' position' Robot
    in  (field'', position')

getObject :: MineState -> Point -> Cell
getObject field (x, y) = field !! y !! x

hasObject :: MineState -> Point -> Cell -> Bool
hasObject field point object = getObject field point == object

updateField :: MineState -> MineState
updateField field =
    let width  = sizeX field
        height = sizeY field
    in  mapi (\row y -> mapi (\cell x ->
                                  updateCell field (x, y)) row) field

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
        -- TODO: Other actions.

replaceCell :: MineState -> Point -> Cell -> MineState
replaceCell field (x, y) cell =
    mapi (\row y' -> mapi (\cell' x' ->
            if (x, y) == (x', y') then cell else cell') row) field

mapi :: (a -> Int -> a) -> [a] -> [a]
mapi fn xs = zipWith fn xs [0..]

updateCell :: MineState -> Point -> Cell
updateCell field (x, y) =
    case getObject field (x, y) of
        ClosedLift | noLambdas field -> OpenLift
        other -> other -- TODO: Falling rocks.

noLambdas :: MineState -> Bool
noLambdas field =
    let width  = sizeX field
        height = sizeY field
    in  all (\p -> getObject field p /= Lambda) [(x, y) | x <- [0..width - 1],
                                                          y <- [1..height - 1]]
