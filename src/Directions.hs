module Directions
        ( directions
        ) where

import Types (Index)

directions :: [Index]
directions = [(x, y) | x <- [-1..1], y <- [-1..1], not (x == 0 && y == 0)]

