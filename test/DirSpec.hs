module DirSpec (main, spec) where

import Test.Hspec

import Directions ( directions )

main :: IO ()
main = hspec $ do
  spec

spec :: SpecWith (Arg Expectation)
spec = describe "Directions" $ do
  it "should have 8 directions (cardinal)" $ do
    length directions `shouldBe` 8

