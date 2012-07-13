module Emulator (
    emulate
) where

import Data.List (findIndex)
import Data.Maybe (fromJust)
import Lifter

lambdaScore = 25

emulate :: GameState -> Action -> (GameState, Bool)
emulate state action =
    let state' = processAction state action
    in  processEnvironment state'

processAction :: GameState -> Action -> GameState
processAction state action =
    let field = gmMineState state
        (x, y) = findRobot field
        (x' , y') = getRobotPos field action (x, y)
        lambdaDelta = if (field !! y' !! x') == Lambda then 1 else 0
        scoreDelta = lambdas * lambdaScore - 1
        field' = replaceCell (replaceCell field (x, y) Empty) (x', y') Robot
        lambdas = gmLambdas state + lambdaDelta
        score = gmScore state + scoreDelta
    in  GameState { gmMineState = field',
                    gmLambdas   = lambdas,
                    gmScore     = score }

processEnvironment :: GameState -> (GameState, Bool)
processEnvironment state = (state, True) -- TODO: fix this ;)

mapi :: (a -> Int -> a) -> [a] -> [a]
mapi fn xs = zipWith fn xs [0..]

replaceCell :: MineState -> (Int, Int) -> Cell -> MineState
replaceCell field (x, y) cell =
    mapi (\row y' -> mapi (\cell' x' ->
            if (x, y) == (x', y') then cell else cell') row) field

findRobot :: MineState -> (Int, Int)
findRobot field =
    let rowIndex = fromJust $ findIndex hasRobot field
        row = (field !! rowIndex)
        cellIndex = findIndex (\c -> c == Robot) row
    in  (rowIndex, fromJust $ cellIndex)
    where hasRobot :: [Cell] -> Bool
          hasRobot cells = any (\c -> c == Robot) cells

getRobotPos :: MineState -> Action -> (Int, Int) -> (Int, Int)
getRobotPos field action (x, y) =
    case action of
        AWait  -> (x, y)
        ALeft  -> (x - 1, y) -- TODO: Check left side
        ARight -> (x + 1, y)
