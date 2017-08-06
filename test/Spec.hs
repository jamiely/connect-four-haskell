import Test.Hspec

import Board (defaultBoard
           , Board(..)
           , isEmpty
           , indicies
           , getMarkerAt
           , setMarkerAt
           , isFull
           , origin
           , render
           )
import Types ( Marker(..)
             )
import Directions ( directions
                  , north
                  )

import Game (checkBoardPos
          , firstEmptyRowIn
          , makeBoardMove
          , initialState
          , GameState(..)
          , makeMove
          , newGame
            )
import Control.Monad.State (evalState
                           )

import Control.Monad (replicateM
                   , liftM)

main :: IO ()
main = hspec $ do
  boardSpec
  dirSpec
  gameSpec

boardSpec :: SpecWith (Arg Expectation)
boardSpec = describe "Board" $ do
  it "should be 7 columns and 6 rows" $ do
    rows defaultBoard `shouldBe` 6
    columns defaultBoard `shouldBe` 7
  it "should have width * height indices" $ do
    length (indicies defaultBoard) `shouldBe` (6 * 7)
  it "should return true for empty check" $ do
    isEmpty defaultBoard `shouldBe` True
  it "should keep track of markers at indices" $ do
    getMarkerAt (0, 0) defaultBoard `shouldBe` Just Empty
    let updatedBoard = setMarkerAt (0, 0) X defaultBoard
    getMarkerAt (0, 0) updatedBoard `shouldBe` Just X
    getMarkerAt (0, 1) updatedBoard `shouldBe` Just Empty
  it "should know when there are no moves left" $ do
    isFull defaultBoard `shouldBe` False
    let is = indicies defaultBoard
    let newBoard = foldl (\b i -> setMarkerAt i X b) defaultBoard is
    isFull newBoard `shouldBe` True
  it "should render board 1" $ do
    let lastB = foldl (\b col -> setMarkerAt (col, 0) X b) defaultBoard [0..4]
    render lastB `shouldBe` "       \n       \n       \n       \n       \nXXXXX  "
  it "should render board 2" $ do
    let lastB = foldl (\b row -> setMarkerAt (0, row) O b) defaultBoard [0..4]
    render lastB `shouldBe` "       \nO      \nO      \nO      \nO      \nO      "

dirSpec :: SpecWith (Arg Expectation)
dirSpec = describe "gamespec is not implemented" $ do
  it "should have 8 directions (cardinal)" $ do
    length directions `shouldBe` 8

gameSpec :: SpecWith (Arg Expectation)
gameSpec = describe "gamespec is not implemented" $ do
  it "should return true when checkBoardPos is called with 0 steps" $ do
    checkBoardPos defaultBoard origin X north 0 `shouldBe` True
  it "should return first empty row" $ do
    firstEmptyRowIn defaultBoard 0 `shouldBe` Just 0
    let board1 = makeBoardMove 0 X defaultBoard
    firstEmptyRowIn board1 0 `shouldBe` Just 1
    let board2 = makeBoardMove 0 X board1
    firstEmptyRowIn board2 0 `shouldBe` Just 2
    let board3 = makeBoardMove 0 X board2
    firstEmptyRowIn board3 0 `shouldBe` Just 3
  it "is easy to make moves" $ do
    let s = liftM last $ replicateM 5 $ makeMove 0
    let finalState = evalState s initialState
    let finalBoard = board finalState
    firstEmptyRowIn finalBoard 0 `shouldBe` Just 5
  it "should return Nothing for the first empty row when the col is full" $ do
    let s = liftM last $ replicateM 6 $ makeMove 0
    let finalState = evalState s initialState
    let finalBoard = board finalState
    firstEmptyRowIn finalBoard 0 `shouldBe` Nothing
  it "should return true when checkBoardPos is called with an index and the marker at that index and 1 step" $ do
    let b1 = defaultBoard
    let b2 = setMarkerAt origin X b1
    checkBoardPos b2 origin X north 1 `shouldBe` True
  it "should return true when checkPosition is called with an index and the marker at that index, the same marker beneath it, and 2 steps" $ do
    let moves = do makeMove 0 -- X
                   makeMove 1 -- O
                   makeMove 0 -- X
    let lastState = evalState moves initialState
    let b = board lastState
    checkBoardPos b origin X north 2 `shouldBe` True

