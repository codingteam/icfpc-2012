module Main (
    main
) where

import Emulator (emulate)
import Lifter

type TestCase = (MineState, Action)

test :: TestCase -> MineState -> IO()
test (field, action) expected =
    let (field', _, _) = emulate field action 0
    in  putStrLn $ "Real:     " ++ (show field') ++ "\n"
                ++ "Expected: " ++ (show expected)

main :: IO()
main = do
    test ([[Robot, Lambda]], ARight) [[Empty, Robot]]
