module Main (
    main
) where

import Emulator (emulate)
import Lifter

main :: IO()
main = do
    putStrLn $ show $ test1 ()

test1 () =
    let field = [[Robot, Lambda]]
        action = ARight
    in  emulate field action 0
