module BoardSpec (main, spec) where

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

main :: IO ()
main = hspec $ do
  spec

spec :: SpecWith (Arg Expectation)
spec = describe "Board" $ do
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

