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

doTest :: Int -> MineState -> Action -> MineState -> Int -> Int -> Bool -> IO()
doTest n field action field' lambdas score finished =
    test n (GameState { gmMineState = field,
                        gmLambdas   = 0,
                        gmScore     = 0,
                        gmFinished  = False }, action)
           GameState { gmMineState = field',
                     gmLambdas   = lambdas,
                     gmScore     = score,
                     gmFinished  = finished }

main :: IO()
main = do
    doTest 1 [[Robot, Lambda]] ARight
             [[Empty, Robot]] 1 24 False
    doTest 2 [[Robot, Lambda, ClosedLift]] ARight
             [[Empty, Robot, OpenLift]] 1 24 False
