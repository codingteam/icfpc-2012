
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
    handleSignals gameState (bruteforce 5 gameState) output

