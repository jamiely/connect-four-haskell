module Main where

import Board

import Control.Monad.State (evalState
                           )
import Game (checkBoardPos
          , firstEmptyRowIn
          , makeBoardMove
          , initialState
          , GameState(..)
          , makeMove
          , newGame
          , isGameOver
          , isWin
            )
import Board (render)

main :: IO ()
main = do
  putStrLn "Play connect four!"
  gameLoop initialState where

  gameLoop states@(g:_) = do
    if isGameOver g then
      showGameOver g
    else
      continuePlaying states

  gameLoop [] = do
    putStrLn "You cannot being a game with no states"

  showGameOver g@(GameState {board=b}) = do
    putStrLn $ render b
    putStrLn "Game over!"
    case isWin g of
      Just m -> putStrLn $ show m ++ " won!!"
      Nothing -> putStrLn "No one won!"

  continuePlaying states@(g@(GameState {board=b}):s) = do
    strCol <- askMove b
    let nextState = evalState (makeMove $ read strCol) states
    gameLoop (nextState:states)

  askMove board = do
    putStrLn $ render board
    putStrLn "In which column do you want to move?"
    strCol <- getLine
    putStrLn $ "You selected column " ++ strCol
    return strCol


