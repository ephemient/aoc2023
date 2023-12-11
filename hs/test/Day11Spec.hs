{-# LANGUAGE OverloadedStrings #-}
module Day11Spec (spec) where

import Data.Text (Text)
import qualified Data.Text as T (unlines)
import Day11 (solve)
import Test.Hspec (Spec, describe, it, shouldBe)

example :: Text
example = T.unlines
  [ "...#......"
  , ".......#.."
  , "#........."
  , ".........."
  , "......#..."
  , ".#........"
  , ".........#"
  , ".........."
  , ".......#.."
  , "#...#....."
  ]

spec :: Spec
spec = do
    describe "part 1" $ do
        it "examples" $ do
            solve 2 example `shouldBe` 374
    describe "part 2" $ do
        it "examples" $ do
            solve 10 example `shouldBe` 1030
            solve 100 example `shouldBe` 8410
