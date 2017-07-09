import Test.Hspec

import Lib (defaultBoard
           , Board(..)
           , isEmpty
           , indicies
           , getMarkerAt
           , setMarkerAt
           , Marker(..)
           , isFull
           )

main :: IO ()
main = hspec $ do
  boardSpec
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

gameSpec :: SpecWith (Arg Expectation)
gameSpec = describe "gamespec is not implemented" $ do
  it "tests if 1 is 1" $ do
    1 `shouldBe` 1
