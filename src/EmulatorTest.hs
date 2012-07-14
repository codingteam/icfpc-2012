module Main (
    main
) where

import Emulator (emulate)
import Lifter

type TestCase = (GameState, Action)

test :: Show a => a -> TestCase -> GameState -> IO()
test n (state, action) expected =
    let result = emulate state action
    in  if result == expected
        then putStrLn $ "Test " ++ (show n) ++ " ok."
        else do putStrLn $ "Test " ++ (show n) ++ " failed."
                putStrLn $ " Real:     " ++ (show result)
                putStrLn $ " Expected: " ++ (show expected)

doTest :: Show a => a -> MineState -> Int -> Int -> Action
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
    -- Movement:
    doTest "up"
        [[Empty], [Robot]] 0 0 AUp
        [[Robot], [Empty]] 0 (-1) False
    doTest "down"
        [[Robot], [Empty]] 0 0 ADown
        [[Empty], [Robot]] 0 (-1) False
    doTest "left"
        [[Empty, Robot]] 0 0 ALeft
        [[Robot, Empty]] 0 (-1) False
    doTest "right"
        [[Robot, Empty]] 0 0 ARight
        [[Empty, Robot]] 0 (-1) False

    -- TODO: Trying to move into an obstacle.
    -- TODO: Trying to move into map egde.

    -- Digging:
    doTest "dig"
        [[Robot, Earth]] 0 0 ARight
        [[Empty, Robot]] 0 (-1) False

    -- TODO: Moving rocks.
    -- TODO: Immovable rocks.

    -- Taking lambdas:
    doTest "take"
        [[Robot, Lambda]] 0 0 ARight
        [[Empty, Robot]] 1 24 False

    -- Opening lifts:
    doTest "open"
        [[Robot, Lambda, ClosedLift]] 0 0 ARight
        [[Empty, Robot, OpenLift]] 1 24 False

    -- TODO: Falling rocks.
    -- TODO: Sliding rocks from a lambdas.
    -- TODO: Destroying rocks with collision.
    -- TODO: Dying under the falling rock.
    -- TODO: Not dying under the stable rock.

    -- Aborting the game:
    doTest "abort"
        [[Robot]] 1 24 AAbort
        [[Robot]] 1 49 True
    -- TODO: Aborting the game in the pre-death conditions.

    -- Winning the game:
    doTest "win"
        [[Robot, OpenLift]] 1 0 ARight
        [[Empty, Robot]] 1 49 True

    -- TODO: Water handling.
