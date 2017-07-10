module Board
    ( defaultBoard
    , Board(..)
    , isEmpty
    , indicies
    , setMarkerAt
    , getMarkerAt
    , isFull
    ) where

import Data.Map as M
import Types (Index, Marker(..))

data Board = Board { rows :: Int
                   , columns :: Int
                   , positions :: M.Map Index Marker
                   } deriving (Show)

defaultBoard :: Board
defaultBoard = createBoard 6 7

createBoard :: Int -> Int -> Board
createBoard rows cols = Board { rows = rows
                              , columns = cols
                              , positions = positions} where
  is = [(c, r) | c <- [0..cols - 1], r <- [0..rows - 1]]
  positions = M.fromList $ zip is (cycle [Empty])

indicies :: Board -> [Index]
indicies board = keys $ positions board

setMarkerAt :: Index -> Marker -> Board -> Board
setMarkerAt index marker Board {rows = rows, columns = columns, positions = positions} = newBoard where
  newBoard = Board rows columns newPositions
  newPositions = insert index marker positions

getMarkerAt :: Index -> Board -> Maybe Marker
getMarkerAt index Board { positions = positions } = M.lookup index positions

markers :: Board -> [Marker]
markers Board { positions = positions } = elems positions

isEmpty :: Board -> Bool
isEmpty board = all (Empty ==) $ markers board

isFull :: Board -> Bool
isFull board = all (Empty /=) $ markers board

