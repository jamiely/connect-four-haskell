module Board
    ( defaultBoard
    , Board(..)
    , isEmpty
    , isEmptyAt
    , indicies
    , columnIndices
    , setMarkerAt
    , getMarkerAt
    , isFull
    , origin
    , orderedIndices
    , render
    ) where

import Data.Map as M
import Types (Index, Marker(..))
import Data.List (intercalate)
import Data.Maybe (maybe)

data Board = Board { rows :: Int
                   , columns :: Int
                   , positions :: M.Map Index Marker
                   } deriving (Show)

origin :: Index
origin = (0, 0)

defaultBoard :: Board
defaultBoard = createBoard 6 7

createBoard :: Int -> Int -> Board
createBoard rows cols = Board { rows = rows
                              , columns = cols
                              , positions = positions} where
  is = [(c, r) | c <- [0..cols - 1], r <- [0..rows - 1]]
  positions = M.fromList $ zip is (cycle [Empty])

orderedIndices :: Int -> Int -> [Index]
orderedIndices rows cols = is where
  is = [(c, r) | c <- [0..cols - 1], r <- [0..rows - 1]]

indicies :: Board -> [Index]
indicies board = keys $ positions board

columnIndices :: Board -> Int -> [Index]
columnIndices (Board { rows = rows, columns = cols }) col =
  [(col, r) | r <- [0..rows -1]]

setMarkerAt :: Index -> Marker -> Board -> Board
setMarkerAt index marker Board {rows = rows, columns = columns, positions = positions} = newBoard where
  newBoard = Board rows columns newPositions
  newPositions = insert index marker positions

getMarkerAt :: Index -> Board -> Maybe Marker
getMarkerAt index Board { positions = positions } = M.lookup index positions

isEmptyAt :: Index -> Board -> Bool
isEmptyAt i b = (getMarkerAt i b) == Just Empty

markers :: Board -> [Marker]
markers Board { positions = positions } = elems positions

isEmpty :: Board -> Bool
isEmpty board = all (Empty ==) $ markers board

isFull :: Board -> Bool
isFull board = all (Empty /=) $ markers board

render :: Board -> String
render b@(Board { rows=rows, columns=cols, positions=positions }) = renderedRows where
  renderRow r = intercalate "" [renderMarker (c, r) | c <- [0..cols-1]]
  renderedRows = intercalate "\n" [renderRow r | r <- [rows-1, rows-2..0]]
  renderMarker index = maybe " " showMarker $ getMarkerAt index b

  showMarker :: Marker -> String
  showMarker Empty = " "
  showMarker X = "X"
  showMarker O = "O"

