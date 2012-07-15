import Core
import Parser
import Emulator

testSeq :: GameState -> [Action] -> GameState
testSeq gameState actions =
    foldl emulate gameState actions

printSeq gameState actions = putStrLn $ show $ testSeq gameState actions

main :: IO ()
main = do
    mine <- readInput
    let actions = [AWait, ARight, ARight, ADown]
        gameState = GameState {
                      gsMineState = mine,
                      gsLambdas   = 0,
                      gsScore     = 0,
                      gsFinished  = False,
                      gsActions   = [] }

    printSeq gameState actions
