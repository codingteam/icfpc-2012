module Main (
    main
) where

import Emulator (emulate)
import Lifter

type TestCase = (GameState, Action)

test :: TestCase -> GameState -> IO()
test (state, action) expected =
    let result = emulate state action
    in  putStrLn $ "Real:     " ++ (show result) ++ "\n"
                ++ "Expected: " ++ (show expected)

main :: IO()
main = do
    test (GameState { gmMineState = [[Robot, Lambda]],
                      gmLambdas   = 0,
                      gmScore     = 0,
                      gmFinished  = False }, ARight)
         GameState { gmMineState = [[Empty, Robot]],
                     gmLambdas   = 1,
                     gmScore     = 24,
                     gmFinished  = False }
