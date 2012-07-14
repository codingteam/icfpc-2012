module Lifter (
    Action   (..),
    Cell     (..),
    GameState(..),
    MineState,
    sizeX, sizeY
) where

data Cell = Empty | Earth | Wall | Rock | Lambda | Robot | ClosedLift | OpenLift
     deriving (Eq, Show)

data Action = ALeft | ARight | AUp | ADown | AWait | AAbort
     deriving (Eq, Show)

type MineState = {
    msField      :: [[Cell]],
    msWater      :: Int,
    msFlooding   :: Int,
    msWaterproof :: Int
} deriving (Eq, Show)

data GameState = GameState {
    gmLambdas   :: Int,
    gmScore     :: Int,
    gmMineState :: MineState,
    gmFinished  :: Bool
} deriving (Eq, Show)

sizeX :: MineState -> Int
sizeX field = let row = field !! 0
              in  length row

sizeY :: MineState -> Int
sizeY field = length field
