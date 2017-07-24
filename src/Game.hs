module Game
        ( checkPosition
        , Game(..)
        , firstEmptyRowIn
        , makeBoardMove
        , initialState
        , GameState(..)
        , makeMove
        ) where

import Types (Marker(..), Index, Direction)
import Board (Board(..)
             , columnIndices
             , isEmptyAt
             , setMarkerAt
             , defaultBoard)
import Data.List (find)
import Control.Monad.State (State(..)
                           , get
                           , put)

data GameState = GameState { current :: Marker
                           , board :: Board
                           } deriving (Show)

type Game = State [GameState] GameState

initialState :: [GameState]
initialState = [GameState { current = X, board = defaultBoard }]

-- | Use to determine whether there are markers in a certain direction
checkPosition :: Board -> Index -> Marker -> Direction -> Int -> Bool
checkPosition _ _ _ _ 0 = True
checkPosition board index marker dir steps = False

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

