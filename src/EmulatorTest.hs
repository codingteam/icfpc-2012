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
        else do putStrLn $ "Test " ++ (show n) ++ ":"
                putStrLn $ " Real:     " ++ (show result)
                putStrLn $ " Expected: " ++ (show expected)

main :: IO()
main = do
    test 1 (GameState { gmMineState = [[Robot, Lambda]],
                      gmLambdas   = 0,
                      gmScore     = 0,
                      gmFinished  = False }, ARight)
         GameState { gmMineState = [[Empty, Robot]],
                     gmLambdas   = 1,
                     gmScore     = 24,
                     gmFinished  = False }
