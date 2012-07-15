module Emulator (
    emulate,
    validate
) where

import Data.List (findIndex)
import Data.Maybe (fromJust)
import Core

type Point = (Int, Int)

lambdaScore = 25
lambdaAbortScore = 25
lambdaLiftScore = 50

emulate :: GameState -> Action -> GameState
emulate state action =
    let actions  = gsActions state
        state'   = processAction state action
        finished = gsFinished state'
        finalState = if finished
                     then state'
                     else processFinishConditions state $ processEnvironment state'

    in  finalState { gsActions = actions ++ [action] }

validate :: GameState -> Action -> Bool
validate gameState action =
    let field = msField $ gsMineState gameState
        robot = findRobot field
    in  action == AWait
        || getRobotPosition field action robot /= robot

processAction :: GameState -> Action -> GameState
processAction gameState action =
    let mineState  = gsMineState gameState
        lambdas    = gsLambdas gameState
        score      = gsScore gameState

        field      = msField mineState

        (field', robotPos') = moveRobot field action
        mineState' = mineState { msField      = field' }

        lambdaDelta = if hasObject field robotPos' Lambda then 1 else 0
        lambdas' = lambdas + lambdaDelta

        scoreDelta  = lambdaDelta * lambdaScore + if action == AAbort
                                                  then lambdas' * lambdaAbortScore
                                                  else -1
        score' = score + scoreDelta

        finished = action == AAbort
    in  gameState { gsMineState = mineState',
                    gsLambdas   = lambdas',
                    gsScore     = score',
                    gsFinished  = finished }

processEnvironment :: GameState -> GameState
processEnvironment state =
    -- TODO: Process flooding.
    let mineState = gsMineState state
        mineState' = updateMineState mineState
    in  state { gsMineState = mineState' }

processFinishConditions :: GameState -> GameState -> GameState
processFinishConditions state state' =
    let mineState  = gsMineState state
        mineState' = gsMineState state'
        lambdas'   = gsLambdas state'
        score'     = gsScore state'

        field      = msField mineState
        field'     = msField mineState'

        robotPosition' = findRobot field'
    in if hasObject field robotPosition' OpenLift
       then state' { gsMineState = mineState',
                     gsLambdas   = lambdas',
                     gsScore     = score' + lambdas' * lambdaLiftScore,
                     gsFinished  = True }
       else let (x, y) = robotPosition'
            in if hasObject field  (x, y - 1) Empty &&
                  hasObject field' (x, y - 1) Rock
                  -- TODO: Check water level and waterproof.
               then state' { gsMineState = mineState',
                             gsLambdas   = lambdas',
                             gsScore     = score',
                             gsFinished  = True }
               else state'

moveRobot :: Field -> Action -> (Field, Point)
moveRobot field action =
    let position  = findRobot field
        position' = getRobotPosition field action position
        field'    = if hasObject field position' Rock
                    then moveRock field position' action
                    else field
        field''   = replaceCell field' position Empty
        field'''  = replaceCell field'' position' Robot
    in  (field''', position')

moveRock field (x, y) action =
    let field' = case action of
                     ALeft  -> replaceCell field (x - 1, y) Rock
                     ARight -> replaceCell field (x + 1, y) Rock
    in  replaceCell field' (x, y) Empty

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

isPassableForRobot :: Field -> Point -> Action -> Bool
isPassableForRobot field point action =
    any (\o -> hasObject field point o) [Empty, Earth, Lambda, OpenLift]
    || (any (\a -> action == a) [ALeft, ARight]
        && hasObject field point Rock && rockMovable field point action)
    where rockMovable :: Field -> Point -> Action -> Bool
          rockMovable field (x, y) action =
              let rockPosition = case action of
                                     ALeft  -> (x - 1, y)
                                     ARight -> (x + 1, y)
              in  hasObject field rockPosition Empty

getRobotPosition :: Field -> Action -> Point -> Point
getRobotPosition field action (x, y) =
    let position = case action of AWait  -> (x, y)
                                  AAbort -> (x, y)
                                  AUp    -> (x, y - 1)
                                  ADown  -> (x, y + 1)
                                  ALeft  -> (x - 1, y)
                                  ARight -> (x + 1, y)
    in  if isPassableForRobot field position action
        then position
        else (x, y)

replaceCell :: Field -> Point -> Cell -> Field
replaceCell field (x, y) cell =
    mapi (\row y' -> mapi (\cell' x' ->
            if (x, y) == (x', y') then cell else cell') row) field

mapi :: (a -> Int -> a) -> [a] -> [a]
mapi fn xs = zipWith fn xs [0..]

updateCell :: Field -> Point -> Cell
updateCell field point =
    case getObject field point of
        ClosedLift | noLambdas field              -> OpenLift
        Rock       | moveRockFromHere field point -> Empty
        Empty      | moveRockHere field point     -> Rock
        other                                     -> other
    where moveRockFromHere field (x, y) =
              -- Free fall:
              hasObject field (x, y + 1) Empty ||
              -- Slide from rock:
              (hasObject field (x, y + 1) Rock &&
               ((hasObject field (x + 1, y) Empty &&
                 hasObject field (x + 1, y + 1) Empty) ||
                 (hasObject field (x - 1, y) Empty &&
                  hasObject field (x - 1, y + 1) Empty))) ||
              -- Slide from lambda:
              (hasObject field (x, y + 1) Lambda &&
               (hasObject field (x + 1, y) Empty &&
                hasObject field (x + 1, y + 1) Empty))
          moveRockHere field (x, y) =
              hasObject field (x, y) Empty &&
              -- Free fall:
              hasObject field (x, y - 1) Rock ||
              -- Slide from a lamdba:
              ((hasObject field (x - 1, y - 1) Rock &&
                hasObject field (x - 1, y) Lambda &&
                hasObject field (x, y - 1) Empty) ||
               -- Slide from a rock (right side):
               (hasObject field (x - 1, y - 1) Rock &&
                hasObject field (x - 1, y) Rock &&
                hasObject field (x, y - 1) Empty) ||
               -- Slide from a rock (left side) only if right side blocked:
               (hasObject field (x, y - 1) Empty &&
                hasObject field (x + 1, y - 1) Rock &&
                hasObject field (x + 1, y) Rock &&
                (not (hasObject field (x + 2, y - 1) Empty) ||
                 not (hasObject field (x + 2, y) Empty))))

noLambdas :: Field -> Bool
noLambdas field =
    let width  = sizeX field
        height = sizeY field
    in  all (\p -> getObject field p /= Lambda) [(x, y) | x <- [0..width - 1],
                                                          y <- [0..height - 1]]
