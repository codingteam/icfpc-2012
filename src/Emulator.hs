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
            else processFinishConditions gameState2 $ processEnvironment gameState2

    in  gameState3 { gsActions = actions ++ [action] }

validate :: GameState -> Action -> Bool
validate gameState action =
    let mineState = gsMineState gameState

        field  = msField mineState
        razors = msRazors mineState

        robot = findRobot field

        wait       = action == AWait
        robotMoved = (fst $ getRobotPosition field action robot) /= robot
        useRazor   = action == ARazor && razors > 0 && haveBeards
        haveBeards =
            let (x, y) = robot
                coords = [(x', y') | x' <- [x - 1..x + 1], y' <- [y - 1..y + 1]]
            in  any (\p -> hasObject field p Beard) coords
    in  wait || robotMoved || useRazor

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

        (mineState', robotPosition') = actRobot mineState action

        lambdaDelta = if hasObject field robotPosition' Lambda then 1 else 0
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
                   && (hasObject field' (x, y - 1) Rock
                       || hasObject field' (x, y - 1) Lambda)) -- Lambda there is a result of horock falling.
                  || (currentWaterproof' <= 0)
               then state' { gsFinished  = True }
               else state'

actRobot :: MineState -> Action -> (MineState, Point)
actRobot mineState action =
    let field = msField mineState
        position  = findRobot field
        (position1, rest) = getRobotPosition field action position
        field1    = if hasObject field position1 Rock
                    then moveRock field position1 action
                    else field

        field2 = emptyCells field1 (position : rest)
        field3 = replaceCell field2 position1 Robot
        mineState3 = mineState { msField = field3 }

        mineState4 = processRobotAct mineState3 action position
    in  (mineState4, position1)
    where emptyCells field points =
              foldl (\field' point -> replaceCell field' point Empty) field points

processRobotAct :: MineState -> Action -> Point -> MineState
processRobotAct mineState action point =
    let field  = msField mineState
        razors = msRazors mineState

        field'  = case action of
                  ARazor | razors > 0 -> razor field point
                  _                   -> field
        razors' = max (razors - 1) 0
    in  mineState { msField  = field',
                    msRazors = razors' }
    where razor field point =
              mapi (\row y -> mapi (\cell x -> cleanBeard cell) row) field
          cleanBeard Beard = Empty
          cleanBeard cell  = cell

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
    let field       = msField mineState
        water       = msWater mineState
        flooding    = msFlooding mineState
        beardGrowth = msBeardGrowth mineState
        turns       = msTurns mineState

        maxX = sizeX field - 1
        maxY = sizeY field - 1

        turns'        = turns + 1
        coords        = [(x, y) | y <- [maxY, maxY - 1..0], x <- [0..maxX]]
        mustGrowBeard = beardGrowth /= 0 && turns' `mod` beardGrowth == 0
        field'        = foldl (\field' p -> updateCell field (field', mustGrowBeard) p) field coords

        water' = if flooding /= 0 && turns' `mod` flooding == 0
                 then water + 1
                 else water

    in  mineState { msField      = field',
                    msWater      = water',
                    msTurns      = turns' }

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

-- getRobotPosition returns target point and any additional points to be cleaned after turn.
getRobotPosition :: Field -> Action -> Point -> (Point, [Point])
getRobotPosition field action (x, y) =
    let position = case action of AWait  -> (x, y)
                                  AAbort -> (x, y)
                                  ARazor -> (x, y)
                                  AUp    -> (x, y - 1)
                                  ADown  -> (x, y + 1)
                                  ALeft  -> (x - 1, y)
                                  ARight -> (x + 1, y)
        cell = getObject field position
    in  if isPassableForRobot field position action
        then case cell of
                 Trampoline (_, key) -> (getTargetPosition field key, [position])
                 _                   -> (position, [])
        else ((x, y), [])

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

moveCell :: Field -> Point -> Point -> Field
moveCell field point1 point2 =
    let cell1  = getObject field point1
        field' = replaceCell field point1 Empty
    in  replaceCell field' point2 cell1

mapi :: (a -> Int -> a) -> [a] -> [a]
mapi fn xs = zipWith fn xs [0..]

updateCell :: Field -> (Field, Bool) -> Point -> Field
updateCell field (field', mustGrowBeard) (x, y) =
    case getObject field (x, y) of
        ClosedLift | noLambdas     -> openLift
        Horock     | mustMoveRock  -> moveHorock ()
        Rock       | mustMoveRock  -> moveRock ()
        Beard      | mustGrowBeard -> growBeard
        _                          -> field'
    where -- Lifts:
          noLambdas =
              let width  = sizeX field
                  height = sizeY field
              in  all (\p -> let cell = getObject field p
                             in  cell /= Lambda && cell /= Horock) [(x, y) | x <- [0..width - 1],
                                                                             y <- [0..height - 1]]

          openLift = replaceCell field' (x, y) OpenLift

          -- Rocks and horocks:
          mustMoveRock  = canFall || canSlide
          canFall       = hasObject field (x, y + 1) Empty
          canSlide      = canSlideLeft || canSlideRight
          canSlideLeft  = (hasObject field (x, y + 1) Rock || hasObject field (x, y + 1) Horock) &&
                          (hasObject field (x - 1, y) Empty && hasObject field (x - 1, y + 1) Empty)
          canSlideRight = (hasObject field (x, y + 1) Rock || hasObject field (x, y + 1) Lambda
                           || hasObject field (x, y + 1) Horock) &&
                          (hasObject field (x + 1, y) Empty && hasObject field (x + 1, y + 1) Empty)

          moveRock () | canFall       = fst fall
          moveRock () | canSlideLeft  = fst slideLeft
          moveRock () | canSlideRight = fst slideRight

          moveHorock () | canFall       = applyHorock fall
          moveHorock () | canSlideLeft  = applyHorock slideLeft
          moveHorock () | canSlideRight = applyHorock slideRight

          fall       = (moveCell field' (x, y) (x,     y + 1), (x,     y + 1))
          slideLeft  = (moveCell field' (x, y) (x - 1, y + 1), (x - 1, y + 1))
          slideRight = (moveCell field' (x, y) (x + 1, y + 1), (x + 1, y + 1))

          applyHorock (f, (x', y')) =
              if not $ hasObject f (x', y' + 1) Empty
              then replaceCell f (x', y') Lambda
              else f

          -- Beards:
          growBeard  = let coords = [(x', y') | x' <- [x - 1..x + 1], y' <- [y - 1..y + 1]]
                       in  foldl beard field' coords
                       where beard f p = let cell = getObject field p
                                         in  if isOnField f p && cell == Empty
                                             then replaceCell f p Beard
                                             else f
