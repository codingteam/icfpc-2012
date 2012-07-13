module Parser where

import Prelude

data Cell = Empty | Earth | Wall | Rock | Lambda | Robot | ClosedLift | OpenLift deriving Show

cell ch = case ch of ' ' -> Empty
                     '.' -> Earth
                     '#' -> Wall
                     '*' -> Rock
                     '\\' -> Lambda
                     'R' -> Robot
                     'L' -> ClosedLift
                     other -> error ("Unknown character: " ++ [other])
