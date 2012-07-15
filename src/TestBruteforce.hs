import Core
import Handler
import Bruteforce

main =
  let gameState = GameState { gsMineState = MineState { msField = [[Wall, ClosedLift, Wall],
                                                                   [Wall, Robot, Wall],
                                                                   [Wall, Lambda, Wall],
                                                                   [Wall, Wall, Wall]],
                                                        msWater = defaultWater,
                                                        msFlooding = defaultFlooding,
                                                        msWaterproof = defaultWaterproof },
                              gsLambdas = 0,
                              gsScore = 0,
                              gsFinished = False,
                              gsActions = [] }
  in handleSignals gameState (bruteforce 5 gameState) output
