import Lifter
import Handler
import Bruteforce

main =
  let gameState = GameState { gmMineState = MineState { msField = [[Wall, ClosedLift, Wall],
                                                                   [Wall, Robot, Wall],
                                                                   [Wall, Lambda, Wall],
                                                                   [Wall, Wall, Wall]],
                                                        msWater = defaultWater,
                                                        msFlooding = defaultFlooding,
                                                        msWaterproof = defaultWaterproof },
                              gmLambdas = 0,
                              gmScore = 0,
                              gmFinished = False,
                              gmActions = [] }
  in handleSignals gameState (bruteforce 5 gameState) output