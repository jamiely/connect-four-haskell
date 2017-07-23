module Directions
        ( directions
        , north
        , south
        , east
        , west
        ) where

import Types (Direction)

directions :: [Direction]
directions = [(x, y) | x <- [-1..1], y <- [-1..1], not (x == 0 && y == 0)]

north :: Direction
north = (0, 1)

south :: Direction
south = (0, -1)

east :: Direction
east = (1, 0)

west :: Direction
west = (1, 0)

