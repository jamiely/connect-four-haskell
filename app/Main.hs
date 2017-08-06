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
            )

main :: IO ()
main = putStrLn $ show b where
  moves = do makeMove 0
             makeMove 0
  lastState = evalState moves initialState
  b = board lastState

