module Lifter (
    Cell(..),
    Action(..),
    MineState
) where

data Cell = Empty | Earth | Wall | Rock | Lambda | Robot | ClosedLift | OpenLift
          deriving (Eq, Show)
data Action = ALeft | ARight | AUp | ADown | AWait | AAbort
            deriving (Eq, Show)

type MineState = [[Cell]]
