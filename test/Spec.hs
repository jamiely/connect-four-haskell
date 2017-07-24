import Test.Hspec

import Board (defaultBoard
           , Board(..)
           , isEmpty
           , indicies
           , getMarkerAt
           , setMarkerAt
           , isFull
           , origin
           )
import Types ( Marker(..)
             )
import Directions ( directions
                  , north
                  )

import Game (checkPosition
          , firstEmptyRowIn
          , makeBoardMove
          , initialState
          , GameState(..)
          , makeMove
            )
import Control.Monad.State (evalState
                           )

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

dirSpec :: SpecWith (Arg Expectation)
dirSpec = describe "gamespec is not implemented" $ do
  it "should have 8 directions (cardinal)" $ do
    length directions `shouldBe` 8

gameSpec :: SpecWith (Arg Expectation)
gameSpec = describe "gamespec is not implemented" $ do
  it "should return true when checkPosition is called with 0 steps" $ do
    checkPosition defaultBoard origin X north 0 `shouldBe` True
  it "should return first empty row" $ do
    firstEmptyRowIn defaultBoard 0 `shouldBe` Just 0
    let board1 = makeBoardMove 0 X defaultBoard
    firstEmptyRowIn board1 0 `shouldBe` Just 1
    let board2 = makeBoardMove 0 X board1
    firstEmptyRowIn board2 0 `shouldBe` Just 2
    let board3 = makeBoardMove 0 X board2
    firstEmptyRowIn board3 0 `shouldBe` Just 3
  it "is easy to make moves" $ do
    let finalState = evalState s initialState
    let finalBoard = board finalState
    firstEmptyRowIn finalBoard 0 `shouldBe` Nothing where
    s = do
      makeMove 0
      makeMove 0
      makeMove 0
      makeMove 0
      makeMove 0
      makeMove 0

