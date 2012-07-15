module Main (
    main
) where

import Emulator (emulate)
import Core

type TestCase = (GameState, Action)

-- Default states:
mineState = MineState { msField             = [],
                        msWater             = defaultWater,
                        msFlooding          = defaultFlooding,
                        msWaterproof        = defaultWaterproof,
                        msBeardGrowth       = defaultBeardGrowth,
                        msCurrentWaterproof = defaultWaterproof,
                        msRazors            = defaultRazors,
                        msTurns             = 0 }

gameState = GameState { gsMineState = mineState,
                        gsLambdas   = 0,
                        gsScore     = 0,
                        gsFinished  = False,
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
           gameState { gsMineState = mineState { msField = field',
                                                 msTurns = 1 },
                       gsLambdas   = lambdas',
                       gsScore     = score',
                       gsFinished  = finished,
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
    test "abort"
        (gameState { gsMineState = mineState { msField = [[Robot]] },
                     gsLambdas   = 1,
                     gsScore     = 24 }, AAbort)
        gameState { gsMineState = mineState { msField = [[Robot]] },
                    gsLambdas   = 1,
                    gsScore     = 49,
                    gsFinished  = True,
                    gsActions   = [AAbort] }

    -- Aborting the game in the pre-death conditions.
    test "emergency abort"
        (gameState { gsMineState = mineState { msField = [[Rock], [Empty], [Robot]] },
                     gsLambdas   = 1,
                     gsScore     = 0 }, AAbort)
        gameState { gsMineState = mineState { msField = [[Rock], [Empty], [Robot]] },
                    gsLambdas   = 1,
                    gsScore     = 25,
                    gsFinished  = True,
                    gsActions   = [AAbort] }

    -- Winning the game:
    doTest "win"
        [[Robot, OpenLift]] 1 0 ARight
        [[Empty, Robot]] 1 49 True

    -- Water handling:
    test "flooding"
        (gameState { gsMineState = mineState { msField    = [[Robot], [Empty]],
                                               msFlooding = 2,
                                               msWater    = 0,
                                               msTurns    = 1 } }, AWait)
        gameState { gsMineState = mineState { msField    = [[Robot], [Empty]],
                                              msFlooding = 2,
                                              msWater    = 1,
                                              msTurns    = 2 },
                    gsScore     = -1,
                    gsActions   = [AWait] }

    -- Decrementing waterproof:
    test "waterproof"
        (gameState { gsMineState = mineState { msField             = [[Robot]],
                                               msWater             = 1,
                                               msCurrentWaterproof = 2 } }, AWait)
        gameState { gsMineState = mineState { msField             = [[Robot]],
                                              msWater             = 1,
                                              msCurrentWaterproof = 1,
                                              msTurns             = 1 },
                    gsScore     = -1,
                    gsActions   = [AWait] }

    -- Drawning:
    test "drawn"
        (gameState { gsMineState = mineState { msField             = [[Robot]],
                                               msWater             = 1,
                                               msCurrentWaterproof = 1 } }, AWait)
        gameState { gsMineState = mineState { msField             = [[Robot]],
                                              msWater             = 1,
                                              msCurrentWaterproof = 0,
                                              msTurns             = 1 },
                    gsScore     = -1,
                    gsFinished  = True,
                    gsActions   = [AWait] }

    -- Waterproof restoring:
    test "waterproof restore"
        (gameState { gsMineState = mineState { msField             = [[Robot]],
                                               msWaterproof        = 10,
                                               msCurrentWaterproof = 1 } }, AWait)
        gameState { gsMineState = mineState { msField             = [[Robot]],
                                              msWaterproof        = 10,
                                              msCurrentWaterproof = 10,
                                              msTurns             = 1 },
                    gsScore     = -1,
                    gsActions   = [AWait] }

    -- Trampoline test:
    doTest "trampoline test"
        [[Robot, Trampoline('A', '1'), Target '1']] 0 0 ARight
        [[Empty, Empty, Robot]] 0 (-1) False

    -- Beard growth test:
    test "beard growth"
        (gameState { gsMineState = mineState { msField       = [[Robot, Beard, Empty]],
                                               msBeardGrowth = 2,
                                               msTurns       = 1 } }, AWait)
        gameState { gsMineState = mineState { msField       = [[Robot, Beard, Beard]],
                                              msBeardGrowth = 2,
                                              msTurns       = 2 },
                    gsScore     = -1,
                    gsActions   = [AWait] }

    -- Beard not growth test:
    test "beard not grow"
        (gameState { gsMineState = mineState { msField       = [[Robot, Beard, Empty]],
                                               msBeardGrowth = 2 } }, AWait)
        gameState { gsMineState = mineState { msField       = [[Robot, Beard, Empty]],
                                              msBeardGrowth = 2,
                                              msTurns       = 1 },
                    gsScore     = -1,
                    gsActions   = [AWait] }

    -- Razor test:
    test "razor"
        (gameState { gsMineState = mineState { msField       = [[Robot, Beard]],
                                               msRazors      = 1 } }, ARazor)
        gameState { gsMineState = mineState { msField       = [[Robot, Empty]],
                                              msRazors      = 0,
                                              msTurns       = 1 },
                    gsScore     = -1,
                    gsActions   = [ARazor] }

    -- Razor test when there are no razors left:
    doTest "no razors"
        [[Robot, Beard]] 0 0 ARazor
        [[Robot, Beard]] 0 (-1) False
