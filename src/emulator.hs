module emulator (
    emulate
) where

import Data.List (findIndex)
import Data.Maybe (fromJust)

lambdaScore = 25

data Cell = Empty | Earth | Wall | Rock | Lambda | Robot | ClosedLift | OpenLift
          deriving (Eq, Show)
data Action = ALeft | ARight | AUp | ADown | AWait | AAbort
            deriving (Eq, Show)

type MineState = [[Cell]]

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

move :: (MineState, Int) -> Action -> (MineState, Int)
move (field, lambdas) action =
    let (x, y) = findRobot field
        (x' , y') = getRobotPos field action (x, y)
        scoreDelta = if (field !! y' !! x') == Lambda then lambdaScore else 0
        field' = replaceCell (replaceCell field (x, y) Empty) (x', y') Robot
    in  (field', scoreDelta - 1) -- -1 for any move, right?

updateEnvironment :: (MineState, Int) -> (MineState, Int, Bool)
updateEnvironment (field, scoreDelta) = (field, scoreDelta, True) -- TODO: fix this ;)

emulate :: MineState -> Action -> Int -> (MineState, Int, Bool)
emulate field action lambdas =
    let (field', scoreDelta, finished) = updateEnvironment $ move (field, lambdas) action
    in  (field', scoreDelta, finished)
