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
emulate gameState action =
    let actions    = gsActions gameState
        gameState1 = processWaterproof gameState
        gameState2 = processAction gameState1 action
        finished2  = gsFinished gameState2
        gameState3 =
            if finished2
            then gameState2
            else processFinishConditions gameState $ processEnvironment gameState2

    in  gameState3 { gsActions = actions ++ [action] }

validate :: GameState -> Action -> Bool
validate gameState action =
    let field = msField $ gsMineState gameState
        robot = findRobot field
    in  action == AWait
        || getRobotPosition field action robot /= robot

processWaterproof :: GameState -> GameState
processWaterproof gameState =
    let mineState  = gsMineState gameState

        field             = msField mineState
        water             = msWater mineState
        waterproof        = msWaterproof mineState
        currentWaterproof = msCurrentWaterproof mineState

        currentWaterproof' = if isRobotInWater field water
                             then currentWaterproof - 1
                             else waterproof
    in  gameState { gsMineState =
                        mineState { msCurrentWaterproof = currentWaterproof' } }

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
processEnvironment gameState =
    let mineState = gsMineState gameState
        mineState' = updateMineState mineState
    in  gameState { gsMineState = mineState' }

processFinishConditions :: GameState -> GameState -> GameState
processFinishConditions state state' =
    let mineState  = gsMineState state
        mineState' = gsMineState state'
        lambdas'   = gsLambdas state'
        score'     = gsScore state'

        field              = msField mineState
        field'             = msField mineState'
        currentWaterproof' = msCurrentWaterproof mineState'

        robotPosition' = findRobot field'
    in if hasObject field robotPosition' OpenLift
       then state' { gsScore     = score' + lambdas' * lambdaLiftScore,
                     gsFinished  = True }
       else let (x, y) = robotPosition'
            in if (hasObject field (x, y - 1) Empty
                   && hasObject field' (x, y - 1) Rock)
                  || (currentWaterproof' <= 0)
               then state' { gsFinished  = True }
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

hasTrampoline :: Field -> Point -> Bool
hasTrampoline field point =
    let onField = isOnField field point
        cell    = getObject field point
        isTrampoline = case cell of
                           Trampoline _ -> True
                           _            -> False
    in  onField && isTrampoline

updateMineState :: MineState -> MineState
updateMineState mineState =
    let field      = msField mineState
        water      = msWater mineState
        flooding   = msFlooding mineState
        turns      = msTurns mineState

        width  = sizeX field
        height = sizeY field

        field' = mapi (\row y -> mapi (\cell x -> updateCell field (x, y)) row) field
        water' = if flooding /= 0 && turns `div` flooding == 0
                 then water + 1
                 else water
    in  mineState { msField      = field',
                    msWater      = water',
                    msTurns      = turns + 1 }

findRobot :: Field -> Point
findRobot field =
    let rowIndex = fromJust $ findIndex hasRobot field
        row = (field !! rowIndex)
        cellIndex = findIndex (\c -> c == Robot) row
    in  (fromJust cellIndex, rowIndex)
    where hasRobot :: [Cell] -> Bool
          hasRobot cells = any (\c -> c == Robot) cells

isRobotInWater :: Field -> Int -> Bool
isRobotInWater field waterLevel =
    let (_, y)     = findRobot field
        height     = sizeY field
        robotLevel = height - y
    in  robotLevel <= waterLevel

isPassableForRobot :: Field -> Point -> Action -> Bool
isPassableForRobot field point action =
    any (\o -> hasObject field point o) [Empty, Earth, Lambda, OpenLift]
    || hasTrampoline field point
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
        cell = getObject field position
    in  if isPassableForRobot field position action
        then case cell of
                 Trampoline (_, key) -> getTargetPosition field key
                 _                   -> position
        else (x, y)

getTargetPosition :: Field -> Char -> Point
getTargetPosition field key =
    let y   = fromJust $ findIndex (\row -> any isTarget row) field
        row = field !! y
        x   = fromJust $ findIndex isTarget row
    in  (x, y)
    where isTarget cell = case cell of
                              Target k | k == key -> True
                              _                   -> False

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
