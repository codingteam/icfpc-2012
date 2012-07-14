module Emulator (
    emulate
) where

import Data.List (findIndex)
import Data.Maybe (fromJust)
import Lifter

type Point = (Int, Int)

lambdaScore = 25
lambdaAbortScore = 25
lambdaLiftScore = 50

emulate :: GameState -> Action -> GameState
emulate state action =
    let state'  = processAction state action
        state'' = processEnvironment state'
    in  processFinishConditions state state''

processAction :: GameState -> Action -> GameState
processAction state action =
    let mineState  = gmMineState state
        lambdas    = gmLambdas state
        score      = gmScore state

        field      = msField mineState
        water      = msWater mineState
        flooding   = msFlooding mineState
        waterproof = msWaterproof mineState


        (field', robotPos') = moveRobot field action
        mineState' = MineState { msField      = field',
                                 msWater      = water,
                                 msFlooding   = flooding,
                                 msWaterproof = waterproof }

        lambdaDelta = if hasObject field robotPos' Lambda then 1 else 0
        lambdas' = lambdas + lambdaDelta

        scoreDelta  = lambdaDelta * lambdaScore + if action == AAbort
                                                  then lambdas' * lambdaAbortScore
                                                  else -1
        score' = score + scoreDelta

        finished = action == AAbort
    in  GameState { gmMineState = mineState',
                    gmLambdas   = lambdas',
                    gmScore     = score',
                    gmFinished  = finished }

processEnvironment :: GameState -> GameState
processEnvironment state =
    -- TODO: Process flooding.
    let finished = gmFinished state
    in  if finished
        then state
        else let mineState = gmMineState state
                 lambdas = gmLambdas state
                 score = gmScore state

                 mineState' = updateMineState mineState
             in  GameState { gmMineState = mineState',
                             gmLambdas   = lambdas,
                             gmScore     = score,
                             gmFinished  = finished }

processFinishConditions :: GameState -> GameState -> GameState
processFinishConditions state state' =
    let mineState  = gmMineState state
        mineState' = gmMineState state'
        lambdas'   = gmLambdas state'
        score'     = gmScore state'

        field      = msField mineState
        field'     = msField mineState'

        robotPosition' = findRobot field'
    in if hasObject field robotPosition' OpenLift
       then GameState { gmMineState = mineState',
                        gmLambdas   = lambdas',
                        gmScore     = score' + lambdas' * lambdaLiftScore,
                        gmFinished  = True }
       else let (x, y) = robotPosition'
            in if hasObject field  (x, y - 1) Empty &&
                  hasObject field' (x, y - 1) Rock
                  -- TODO: Check water level and waterproof.
               then GameState { gmMineState = mineState,
                                gmLambdas   = lambdas',
                                gmScore     = score',
                                gmFinished  = True }
               else state'

moveRobot :: Field -> Action -> (Field, Point)
moveRobot field action =
    let position  = findRobot field
        position' = getRobotPosition field action position
        field'    = replaceCell field position Empty
        field''   = replaceCell field' position' Robot
        -- TODO: Moving rocks.
    in  (field'', position')

getObject :: Field -> Point -> Cell
getObject field (x, y) = field !! y !! x

isOnField :: Field -> Point -> Bool
isOnField field (x, y) = x >= 0 && y >= 0 && x < sizeX field && y < sizeY field

hasObject :: Field -> Point -> Cell -> Bool
hasObject field point object =
    isOnField field point && getObject field point == object

updateMineState :: MineState -> MineState
updateMineState mineState =
    let field      = msField mineState
        water      = msWater mineState
        flooding   = msFlooding mineState
        waterproof = msWaterproof mineState

        width  = sizeX field
        height = sizeY field

        field' = mapi (\row y -> mapi (\cell x -> updateCell field (x, y)) row) field
        -- TODO: Calculate water level and waterproof.
    in  MineState { msField      = field',
                    msWater      = water,
                    msFlooding   = flooding,
                    msWaterproof = waterproof }

findRobot :: Field -> Point
findRobot field =
    let rowIndex = fromJust $ findIndex hasRobot field
        row = (field !! rowIndex)
        cellIndex = findIndex (\c -> c == Robot) row
    in  (fromJust cellIndex, rowIndex)
    where hasRobot :: [Cell] -> Bool
          hasRobot cells = any (\c -> c == Robot) cells

isPassableForRobot :: Field -> Point -> Bool
isPassableForRobot field point =
    any (\o -> hasObject field point o) [Empty, Earth, Lambda, OpenLift]
    -- TODO: Check for moving rocks.

getRobotPosition :: Field -> Action -> Point -> Point
getRobotPosition field action (x, y) =
    let position = case action of AWait  -> (x, y)
                                  AAbort -> (x, y)
                                  AUp    -> (x, y - 1)
                                  ADown  -> (x, y + 1)
                                  ALeft  -> (x - 1, y)
                                  ARight -> (x + 1, y)
    in  if isPassableForRobot field position
        then position
        else (x, y)

replaceCell :: Field -> Point -> Cell -> Field
replaceCell field (x, y) cell =
    mapi (\row y' -> mapi (\cell' x' ->
            if (x, y) == (x', y') then cell else cell') row) field

mapi :: (a -> Int -> a) -> [a] -> [a]
mapi fn xs = zipWith fn xs [0..]

updateCell :: Field -> Point -> Cell
updateCell field (x, y) =
    case getObject field (x, y) of
        ClosedLift | noLambdas field -> OpenLift
        other -> other -- TODO: Falling rocks.

noLambdas :: Field -> Bool
noLambdas field =
    let width  = sizeX field
        height = sizeY field
    in  all (\p -> getObject field p /= Lambda) [(x, y) | x <- [0..width - 1],
                                                          y <- [0..height - 1]]
