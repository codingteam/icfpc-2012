module Main (
    main
) where

import Emulator (emulate)
import Core

type TestCase = (GameState, Action)

-- Default states:
mineState = MineState { msField      = [],
                        msWater      = defaultWater,
                        msFlooding   = defaultFlooding,
                        msWaterproof = defaultWaterproof }

gameState = GameState { gsMineState = mineState,
                        gsLambdas   = 0,
                        gsScore     = 0,
                        gsFinished  = False,
                        gsTurns     = 0,
                        gsActions   = [] }

-- Test functions:
test :: Show a => a -> TestCase -> GameState -> IO()
test n (state, action) expected =
    let result = emulate state action
    in  if result == expected
        then putStrLn $ "Test " ++ (show n) ++ " ok."
        else do putStrLn $ "Test " ++ (show n) ++ " failed."
                putStrLn $ " Real:     " ++ (show result)
                putStrLn $ " Expected: " ++ (show expected)

doTest :: Show a => a -> Field -> Int -> Int -> Action
       -> Field -> Int -> Int -> Bool -> IO()
doTest n field lambdas score action field' lambdas' score' finished =
    test n (gameState { gsMineState = mineState { msField = field },
                        gsLambdas   = lambdas,
                        gsScore     = score,
                        gsFinished  = False,
                        gsActions   = [] }, action)
           gameState { gsMineState = mineState { msField = field' },
                       gsLambdas   = lambdas',
                       gsScore     = score',
                       gsFinished  = finished,
                       gsTurns     = 1,
                       gsActions   = [action] }

-- Entry point:
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

    -- Waiting:
    doTest "wait"
        [[Robot, Empty]] 0 0 AWait
        [[Robot, Empty]] 0 (-1) False

    -- Blocked movement:
    doTest "blocked by wall"
        [[Robot, Wall]] 0 0 ARight
        [[Robot, Wall]] 0 (-1) False
    doTest "blocked by rock"
        [[Robot, Rock]] 0 0 ARight
        [[Robot, Rock]] 0 (-1) False
    doTest "blocked by rock blocked by lambda"
        [[Robot, Rock, Lambda]] 0 0 ARight
        [[Robot, Rock, Lambda]] 0 (-1) False

    -- Blocked by map edge:
    doTest "map edge"
        [[Empty, Robot]] 0 0 ARight
        [[Empty, Robot]] 0 (-1) False

    -- Digging:
    doTest "dig"
        [[Robot, Earth]] 0 0 ARight
        [[Empty, Robot]] 0 (-1) False

    -- Moving rocks:
    doTest "moving rock"
        [[Robot, Rock, Empty]] 0 0 ARight
        [[Empty, Robot, Rock]] 0 (-1) False

    -- Immovable rocks:
    doTest "rock blocked by lambda"
        [[Robot, Rock, Lambda]] 0 0 ARight
        [[Robot, Rock, Lambda]] 0 (-1) False

    -- Taking lambdas:
    doTest "take"
        [[Robot, Lambda]] 0 0 ARight
        [[Empty, Robot]] 1 24 False

    -- Opening lifts:
    doTest "open"
        [[Robot, Lambda, ClosedLift]] 0 0 ARight
        [[Empty, Robot, OpenLift]] 1 24 False

    -- Falling rocks:
    doTest "fall"
        [[Empty, Rock, Rock], [Robot, Empty, Empty]] 0 0 AWait
        [[Empty, Empty, Empty], [Robot, Rock, Rock]] 0 (-1) False

    -- Two rocks falling:
    doTest "two rocks"
        [[Robot, Rock], [Empty, Rock], [Empty, Empty]] 0 0 AWait
        [[Robot, Rock], [Empty, Empty], [Empty, Rock]] 0 (-1) False

    -- Sliding rocks:
    doTest "slide rock from a lambda"
        [[Robot, Rock, Empty], [Empty, Lambda, Empty]] 0 0 AWait
        [[Robot, Empty, Empty], [Empty, Lambda, Rock]] 0 (-1) False

    doTest "no slide from a lambda in left direction"
        [[Robot, Empty, Rock], [Empty, Empty, Lambda]] 0 0 AWait
        [[Robot, Empty, Rock], [Empty, Empty, Lambda]] 0 (-1) False

    doTest "slide rock from a rock (left)"
        [[Robot, Empty, Rock], [Empty, Empty, Rock]] 0 0 AWait
        [[Robot, Empty, Empty], [Empty, Rock, Rock]] 0 (-1) False

    doTest "slide rock from a rock (right)"
        [[Robot, Rock, Empty], [Empty, Rock, Empty]] 0 0 AWait
        [[Robot, Empty, Empty], [Empty, Rock, Rock]] 0 (-1) False

    doTest "slide blocked by obstacle"
        [[Robot, Rock, Wall], [Empty, Rock, Empty]] 0 0 AWait
        [[Robot, Rock, Wall], [Empty, Rock, Empty]] 0 (-1) False

    -- Destroying rocks with a collision:
    doTest "rock collision"
        [[Robot, Rock, Empty, Rock], [Empty, Rock, Empty, Rock]] 0 0 AWait
        [[Robot, Empty, Empty, Empty], [Empty, Rock, Rock, Rock]] 0 (-1) False

    -- Dying under a rock:
    doTest "die"
        [[Rock], [Empty], [Robot]] 0 0 AWait
        [[Empty], [Rock], [Robot]] 0 (-1) True

    -- Not dying under the stable rock:
    doTest "alive"
        [[Rock], [Robot]] 0 0 AWait
        [[Rock], [Robot]] 0 (-1) False

    -- Aborting the game:
    doTest "abort"
        [[Robot]] 1 24 AAbort
        [[Robot]] 1 49 True

    -- Aborting the game in the pre-death conditions.
    doTest "emergency abort"
        [[Rock], [Empty], [Robot]] 1 0 AAbort
        [[Rock], [Empty], [Robot]] 1 25 True

    -- Winning the game:
    doTest "win"
        [[Robot, OpenLift]] 1 0 ARight
        [[Empty, Robot]] 1 49 True

    -- Water handling:
    test "flooding"
        (gameState { gsMineState = mineState { msField    = [[Robot], [Empty]],
                                               msFlooding = 2,
                                               msWater    = 0 },
                     gsTurns     = 1 }, AWait)
        gameState { gsMineState = mineState { msField    = [[Robot], [Empty]],
                                              msFlooding = 2,
                                              msWater    = 1 },
                    gsTurns     = 2,
                    gsActions   = [AWait] }

    -- Decrementing waterproof:
    test "waterproof"
        (gameState { gsMineState = mineState { msField      = [[Robot]],
                                               msWater      = 1,
                                               msWaterproof = 2 } }, AWait)
        gameState { gsMineState = mineState { msField      = [[Robot]],
                                              msWater      = 1,
                                              msWaterproof = 1 },
                    gsTurns     = 1,
                    gsActions   = [AWait] }

    -- Drawning:
    test "drawn"
        (gameState { gsMineState = mineState { msField      = [[Robot]],
                                               msWater      = 1,
                                               msWaterproof = 0 } }, AWait)
        gameState { gsMineState = mineState { msField      = [[Robot]],
                                              msWater      = 1,
                                              msWaterproof = 1 },
                    gsFinished  = True,
                    gsTurns     = 1,
                    gsActions   = [AWait] }
