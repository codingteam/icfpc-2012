
import Core
import Parser
import Handler
import Bruteforce

main :: IO ()
main = do
    mine <- readInput
    let gameState = GameState {
                      gsMineState = mine,
                      gsLambdas   = 0,
                      gsScore     = 0,
                      gsFinished  = False,
                      gsActions   = [] }
    let n = sizeX $ msField $ gsMineState gameState
    let m = sizeY $ msField $ gsMineState gameState
    let steps = (n * m) `div` 3
    handleSignals gameState (bruteforce steps gameState) output

