module Parser where

import Prelude
import Lifter

cell ch = case ch of ' ' -> Empty
                     '.' -> Earth
                     '#' -> Wall
                     '*' -> Rock
                     '\\' -> Lambda
                     'R' -> Robot
                     'L' -> ClosedLift
                     other -> error ("Unknown character: " ++ [other])

parseRow row = map cell row

readRow () =
    do
      chars <- getLine
      return (parseRow chars)

bound row = all (\ c -> c == Wall || c == ClosedLift) row

--Don't know if just reading endlessly will work, but i guess it will read only what it'll need
readMap () =
    (readRow ()) : (readMap ())
