module Game
        ( checkPosition
        , Game(..)
        , firstEmptyRowIn
        , makeMove
        ) where

import Types (Marker, Index, Direction)
import Board (Board(..)
             , columnIndices
             , isEmptyAt
             , setMarkerAt)
import Data.List (find)

data Game = Game { current :: Marker
                 , board :: Board
                 } deriving (Show)


-- | Use to determine whether there are markers in a certain direction
checkPosition :: Board -> Index -> Marker -> Direction -> Int -> Bool
checkPosition _ _ _ _ 0 = True
checkPosition board index marker dir steps = False

firstEmptyRowIn :: Board -> Int -> Maybe Int
firstEmptyRowIn board@(Board { positions = ps }) col = fmap snd found where
  found = find (\i -> isEmptyAt i board) $ columnIndices board col

makeMove :: Int -> Marker -> Board -> Board
makeMove col m board = newBoard where
  newBoard = maybe board (\r -> setMarkerAt (col, r) m board) emptyRow
  emptyRow = firstEmptyRowIn board col
