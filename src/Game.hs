module Game
        ( checkBoardPos
        , Game(..)
        , firstEmptyRowIn
        , makeBoardMove
        , initialState
        , GameState(..)
        , makeMove
        , newGame
        , isGameOver
        , isWin
        ) where

import Types (Marker(..), Index, Direction)
import Directions (indexInDirection
                  , directions)
import Board (Board(..)
             , columnIndices
             , isEmptyAt
             , isFull
             , setMarkerAt
             , indicies
             , getMarkerAt
             , defaultBoard)
import Data.List (find)
import Data.Maybe (catMaybes, isJust)
import Control.Monad.State (State(..)
                           , guard
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

winAtIndex :: Board -> Index -> Maybe Marker
winAtIndex board i = result where
  maybeMarker = getMarkerAt i board
  result = do
    m <- maybeMarker
    guard $ m /= Empty
    guard $ checkMarker m
    return m
  checkMarker :: Marker -> Bool
  checkMarker m = any id [ checkBoardPos board i m dir 4 | dir <- directions ]

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
  let marker = if newBoard == board then
                 c
               else
                 nextMarker c
  let lastState = GameState { current = marker, board = newBoard }
  put $ lastState:gameStates
  return lastState

isWin :: GameState -> Maybe Marker
isWin (GameState { board=board }) = result where
  wins = catMaybes $ map (winAtIndex board) $ indicies board
  result = find (const True) wins

isGameOver :: GameState -> Bool
isGameOver g@(GameState { board=board }) = isFull board || isJust (isWin g)

