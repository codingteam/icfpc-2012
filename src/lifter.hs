
import Core
import Parser
import Handler
import Bruteforce

main :: IO ()
main = do
    mine <- readInput
    let gameState = GameState {
                      gmMineState = mine,
                      gmLambdas   = 0,
                      gmScore     = 0,
                      gmFinished  = False,
                      gmActions   = [] }
    handleSignals gameState (bruteforce 5 gameState) output

