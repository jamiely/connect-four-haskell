module Game
        ( checkBoardPos
        , Game(..)
        , firstEmptyRowIn
        , makeBoardMove
        , initialState
        , GameState(..)
        , makeMove
        , newGame
        ) where

import Types (Marker(..), Index, Direction)
import Directions (indexInDirection)
import Board (Board(..)
             , columnIndices
             , isEmptyAt
             , setMarkerAt
             , getMarkerAt
             , defaultBoard)
import Data.List (find)
import Control.Monad.State (State(..)
                           , get
                           , put)

data GameState = GameState { current :: Marker
                           , board :: Board
                           } deriving (Show)

type Game = State [GameState] GameState

originalState = GameState { current = X, board = defaultBoard }

initialState :: [GameState]
initialState = [originalState]

newGame :: Game
newGame = do
  let s = originalState
  put [originalState]
  return s

-- | Use to determine whether there are markers in a certain direction
checkBoardPos :: Board -> Index -> Marker -> Direction -> Int -> Bool
checkBoardPos _ _ _ _ 0 = True
checkBoardPos board index marker _ 1 = m == Just marker where
  m = getMarkerAt index board
checkBoardPos board index marker dir steps = m == Just marker && next where
  nextIndex = indexInDirection dir index
  m = getMarkerAt index board
  next = checkBoardPos board nextIndex marker dir $ steps - 1

firstEmptyRowIn :: Board -> Int -> Maybe Int
firstEmptyRowIn board@(Board { positions = ps }) col = fmap snd found where
  found = find (\i -> isEmptyAt i board) $ columnIndices board col

makeBoardMove :: Int -> Marker -> Board -> Board
makeBoardMove col m board = newBoard where
  newBoard = maybe board (\r -> setMarkerAt (col, r) m board) emptyRow
  emptyRow = firstEmptyRowIn board col

nextMarker :: Marker -> Marker
nextMarker X = O
nextMarker O = X
nextMarker Empty = error "Cannot get nextMarker for Empty"

makeMove :: Int -> Game
makeMove col = do
  gameStates <- get
  let GameState { current = c, board = board } = head gameStates
  let newBoard = makeBoardMove col c board
  let lastState = GameState { current = nextMarker c, board = newBoard }
  put $ lastState:gameStates
  return lastState

