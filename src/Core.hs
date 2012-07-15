module Core (
    Action   (..),
    Cell     (..),
    Field,
    GameState(..),
    MineState(..),
    defaultWater, defaultFlooding, defaultWaterproof, defaultBeardGrowth, defaultRazors,
    sizeX, sizeY
) where

data Cell = Empty | Earth | Wall | Rock | Lambda | Robot | ClosedLift | OpenLift | Beard | Razor
            | Trampoline (Char, Char) -- key, targetKey
            | Target Char -- key
     deriving (Eq, Show)

data Action = ALeft | ARight | AUp | ADown | AWait | AAbort | ARazor
     deriving (Eq, Show)

type Field = [[Cell]]

data MineState = MineState {
    msField             :: Field,
    msWater             :: Int,
    msFlooding          :: Int,
    msWaterproof        :: Int,
    msBeardGrowth       :: Int,
    msCurrentWaterproof :: Int,
    msRazors            :: Int,
    msTurns             :: Int
} deriving (Eq, Show)

data GameState = GameState {
    gsLambdas   :: Int,
    gsScore     :: Int,
    gsMineState :: MineState,
    gsFinished  :: Bool,
    gsActions   :: [Action]
} deriving (Eq, Show)

defaultWater :: Int
defaultWater = 0

defaultFlooding :: Int
defaultFlooding = 0

defaultWaterproof :: Int
defaultWaterproof = 10

defaultBeardGrowth :: Int
defaultBeardGrowth = 25

defaultRazors :: Int
defaultRazors = 0

sizeX :: Field -> Int
sizeX field = let row = field !! 0
              in  length row

sizeY :: Field -> Int
sizeY field = length field
