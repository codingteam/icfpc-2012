module Main (
    main
) where

import Emulator (emulate)
import Lifter

type TestCase = (GameState, Action)

test :: Int -> TestCase -> GameState -> IO()
test n (state, action) expected =
    let result = emulate state action
    in  if result == expected
        then putStrLn $ "Test " ++ (show n) ++ " ok."
        else do putStrLn $ "Test " ++ (show n) ++ " failed."
                putStrLn $ " Real:     " ++ (show result)
                putStrLn $ " Expected: " ++ (show expected)

doTest :: Int -> MineState -> Int -> Int -> Action
       -> MineState -> Int -> Int -> Bool -> IO()
doTest n field lambdas score action field' lambdas' score' finished =
    test n (GameState { gmMineState = field,
                        gmLambdas   = lambdas,
                        gmScore     = score,
                        gmFinished  = False }, action)
           GameState { gmMineState = field',
                       gmLambdas   = lambdas',
                       gmScore     = score',
                       gmFinished  = finished }

main :: IO()
main = do
    doTest 1 [[Robot, Lambda]] 0 0 ARight
             [[Empty, Robot]] 1 24 False
    doTest 2 [[Robot, Lambda, ClosedLift]] 0 0 ARight
             [[Empty, Robot, OpenLift]] 1 24 False
    doTest 3 [[Robot]] 1 24 AAbort
             [[Robot]] 1 49 True
