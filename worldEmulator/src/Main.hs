module Main (
    main
) where

import Data.List (findIndex)
import Data.Maybe (fromJust)

lambdaScore = 25

data Cell = Empty | Earth | Wall | Rock | Lambda | Robot | ClosedLift | OpenLift
          deriving (Eq, Show)
data Action = ALeft | ARight | AUp | ADown | AWait | AAbort
            deriving (Eq, Show)

mapi :: (a -> Int -> a) -> [a] -> [a]
mapi fn xs = zipWith fn xs [0..]

replaceCell :: [[Cell]] -> (Int, Int) -> Cell -> [[Cell]]
replaceCell field (x, y) cell =
    mapi (\row y' -> mapi (\cell' x' ->
            if (x, y) == (x', y') then cell else cell') row) field

findRobot :: [[Cell]] -> (Int, Int)
findRobot field =
    let rowIndex = fromJust $ findIndex hasRobot field
        row = (field !! rowIndex)
        cellIndex = findIndex (\c -> c == Robot) row
    in  (rowIndex, fromJust $ cellIndex)
    where hasRobot :: [Cell] -> Bool
          hasRobot cells = any (\c -> c == Robot) cells

getRobotPos :: [[Cell]] -> Action -> Int -> Int -> (Int, Int)
getRobotPos field action x y =
    case action of
        AWait  -> (x, y)
        ALeft  -> (x - 1, y) -- TODO: Check left side
        ARight -> (x + 1, y)

move :: ([[Cell]], Int) -> Action -> ([[Cell]], Int)
move (field, lambdas) action =
    let (x, y) = findRobot field
        (x' , y') = getRobotPos field action x y
        scoreDelta = if (field !! x' !! y') == Lambda then lambdaScore else 0
        field' = replaceCell (replaceCell field (x, y) Empty) (x', y') Robot
    in  (field', scoreDelta - 1) -- -1 for any move, right?

updateEnvironment :: ([[Cell]], Int) -> ([[Cell]], Int, Bool)
updateEnvironment (field, scoreDelta) = (field, scoreDelta, True) -- TODO: fix this ;)

emulate :: [[Cell]] -> Action -> Int -> ([[Cell]], Int, Bool)
emulate field action lambdas =
    let (field', scoreDelta, finished) = updateEnvironment $ move (field, lambdas) action
    in  (field', scoreDelta, finished)

main :: IO()
main =
    putStrLn $ show $ test1 ()

test1 () =
    let field = [[Robot, Lambda]]
        action = ARight
    in (emulate field action 0)
